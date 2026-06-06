const DB_PATH = "db/prospect_comps_site.sqlite";
const SQL_JS_PATH = "https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.10.3/";

const state = {
  db: null,
  selectedId: null,
};

const els = {
  search: document.getElementById("player-search"),
  status: document.getElementById("status-line"),
  resultCount: document.getElementById("result-count"),
  results: document.getElementById("player-results"),
  empty: document.getElementById("empty-state"),
  comparison: document.getElementById("comparison-view"),
  selectedName: document.getElementById("selected-name"),
  selectedMeta: document.getElementById("selected-meta"),
  comparisonBody: document.getElementById("comparison-body"),
};

function rowsFromResult(result) {
  if (!result.length) return [];
  const { columns, values } = result[0];
  return values.map((valueRow) => Object.fromEntries(columns.map((column, index) => [column, valueRow[index]])));
}

function query(sql, params = []) {
  const statement = state.db.prepare(sql);
  statement.bind(params);
  const rows = [];

  while (statement.step()) {
    rows.push(statement.getAsObject());
  }

  statement.free();
  return rows;
}

function formatNumber(value, digits = 0) {
  if (value === null || value === undefined || Number.isNaN(Number(value))) return "-";
  return Number(value).toLocaleString(undefined, {
    maximumFractionDigits: digits,
    minimumFractionDigits: digits,
  });
}

function setStatus(message, isError = false) {
  els.status.textContent = message;
  els.status.classList.toggle("error", isError);
}

function renderResults(rows) {
  els.resultCount.textContent = rows.length.toString();
  els.results.innerHTML = "";

  if (!rows.length) {
    const empty = document.createElement("div");
    empty.className = "empty-state";
    empty.innerHTML = "<p>No matching player profiles found.</p>";
    els.results.appendChild(empty);
    return;
  }

  const fragment = document.createDocumentFragment();

  for (const row of rows) {
    const button = document.createElement("button");
    button.className = "result-button";
    button.type = "button";
    button.dataset.id = row.batter;

    if (row.batter === state.selectedId) {
      button.classList.add("active");
    }

    const years = row.min_year === row.max_year ? row.min_year : `${row.min_year}-${row.max_year}`;
    button.innerHTML = `
      <span>
        <span class="result-name">${row.name}</span>
        <span class="result-detail">${years} | ${formatNumber(row.career_pa)} PA | Age ${formatNumber(row.avg_age, 1)}</span>
      </span>
      <span class="level-badge">${row.latest_level || "-"}</span>
    `;
    button.addEventListener("click", () => selectPlayer(row.batter));
    fragment.appendChild(button);
  }

  els.results.appendChild(fragment);
}

function searchPlayers(term) {
  const trimmed = term.trim();

  if (trimmed.length < 2) {
    els.resultCount.textContent = "0";
    els.results.innerHTML = "";
    return;
  }

  const rows = query(
    `
    SELECT batter, name, career_pa, avg_age, min_year, max_year, latest_level
    FROM player_profiles
    WHERE name LIKE ?
    ORDER BY
      CASE WHEN name LIKE ? THEN 0 ELSE 1 END,
      name COLLATE NOCASE,
      career_pa DESC
    LIMIT 60
    `,
    [`%${trimmed}%`, `${trimmed}%`]
  );

  renderResults(rows);
}

function renderSelectedMeta(player) {
  const items = [
    ["Latest", player.latest_level],
    ["Levels", player.levels],
    ["Years", player.min_year === player.max_year ? player.min_year : `${player.min_year}-${player.max_year}`],
    ["PA", formatNumber(player.career_pa)],
    ["Age", formatNumber(player.avg_age, 1)],
  ];

  els.selectedMeta.innerHTML = items.map(([label, value]) => `
    <div>
      <dt>${label}</dt>
      <dd>${value}</dd>
    </div>
  `).join("");
}

function renderComparisons(rows) {
  els.comparisonBody.innerHTML = rows.map((row) => `
    <tr>
      <td>${row.rank}</td>
      <td>${row.comp_name}</td>
      <td class="score">${formatNumber(row.similarity_score, 1)}</td>
      <td>${formatNumber(row.comp_career_pa)}</td>
      <td>${row.comp_levels || "-"}</td>
      <td><span class="selected-value">${formatNumber(row.selected_zbb, 2)}</span><span class="comp-value">${formatNumber(row.comp_zbb, 2)}</span></td>
      <td><span class="selected-value">${formatNumber(row.selected_zk, 2)}</span><span class="comp-value">${formatNumber(row.comp_zk, 2)}</span></td>
      <td><span class="selected-value">${formatNumber(row.selected_ziso, 2)}</span><span class="comp-value">${formatNumber(row.comp_ziso, 2)}</span></td>
      <td><span class="selected-value">${formatNumber(row.selected_speed, 1)}</span><span class="comp-value">${formatNumber(row.comp_speed, 1)}</span></td>
    </tr>
  `).join("");
}

function selectPlayer(batter) {
  state.selectedId = batter;

  const player = query(
    `
    SELECT *
    FROM player_profiles
    WHERE batter = ?
    LIMIT 1
    `,
    [batter]
  )[0];

  if (!player) return;

  const comparisons = query(
    `
    SELECT *
    FROM hitter_career_similarity
    WHERE batter = ?
    ORDER BY rank
    LIMIT 10
    `,
    [batter]
  );

  els.empty.classList.add("hidden");
  els.comparison.classList.remove("hidden");
  els.selectedName.textContent = player.name;
  renderSelectedMeta(player);
  renderComparisons(comparisons);

  for (const button of els.results.querySelectorAll(".result-button")) {
    button.classList.toggle("active", button.dataset.id === batter);
  }
}

function debounce(fn, wait = 120) {
  let handle;
  return (...args) => {
    window.clearTimeout(handle);
    handle = window.setTimeout(() => fn(...args), wait);
  };
}

async function init() {
  try {
    const SQL = await initSqlJs({ locateFile: (file) => `${SQL_JS_PATH}${file}` });
    const response = await fetch(DB_PATH);

    if (!response.ok) {
      throw new Error(`Database request failed with ${response.status}`);
    }

    const buffer = await response.arrayBuffer();
    state.db = new SQL.Database(new Uint8Array(buffer));

    const metadata = rowsFromResult(state.db.exec("SELECT value FROM metadata WHERE key = 'scoring_version' LIMIT 1"));
    const version = metadata[0]?.value || "ready";
    setStatus(`Database loaded: ${version}`);
    els.search.disabled = false;
    els.search.focus();
  } catch (error) {
    console.error(error);
    setStatus("Could not load the browser database. Check that db/prospect_comps_site.sqlite was published with the site.", true);
  }
}

els.search.addEventListener("input", debounce((event) => {
  searchPlayers(event.target.value);
}));

init();
