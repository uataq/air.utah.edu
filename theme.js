/* Light or dark theme for air.utah.edu, chosen with the moon/sun button.
 *
 * The choice is kept in the browser's localStorage under 'uataq-theme'
 * ('light' or 'dark'). Light is the default: nothing stored means light,
 * whatever the computer's own setting. The Shiny apps under /s/ load this same
 * file, and they share localStorage with the site (same address), so one click
 * sets the theme everywhere.
 *
 * Loaded in <head> without defer, so the page is dark before it is drawn.
 * Colors for the page itself are in the CSS (html[data-theme="dark"] rules);
 * this file recolors what CSS cannot reach: plotly plots and the map tiles.
 */
(function () {
  var KEY = 'uataq-theme';
  var root = document.documentElement;

  function stored() {
    try { return localStorage.getItem(KEY); } catch (e) { return null; }
  }
  function isDark() { return root.getAttribute('data-theme') === 'dark'; }

  if (stored() === 'dark') root.setAttribute('data-theme', 'dark');

  // ---- plotly -------------------------------------------------------------
  // Dark colors for the plot frame; the plot and paper are made transparent
  // so the page's background shows through.
  var DARK = { font: '#d0d0d0', grid: '#3b3b3b', line: '#5a5a5a',
               menu: '#333333', menuBorder: '#555555' };

  // A trace color too dark to see on a dark background, lightened instead
  function lighten(color) {
    var m, rgb;
    if (typeof color !== 'string') return null;
    if (color === 'black') rgb = [0, 0, 0];
    else if ((m = color.match(/^#([0-9a-f]{6})$/i)))
      rgb = [0, 2, 4].map(function (i) { return parseInt(m[1].substr(i, 2), 16); });
    else if ((m = color.match(/^rgba?\(([^)]+)\)$/)))
      rgb = m[1].split(',').slice(0, 3).map(Number);
    else return null;
    var lum = (0.299 * rgb[0] + 0.587 * rgb[1] + 0.114 * rgb[2]) / 255;
    if (lum > 0.35) return null;
    var alpha = (m && color.indexOf('rgba') === 0) ? m[1].split(',')[3] : 1;
    var mix = rgb.map(function (c) { return Math.round(c + (255 - c) * 0.6); });
    return 'rgba(' + mix.join(',') + ',' + alpha + ')';
  }

  // A pale color in a heatmap's scale (the QAQC heatmap's light green
  // "passing"), dimmed toward the dark background so it does not glare
  function dim(color) {
    var m = typeof color === 'string' && color.match(/^#([0-9a-f]{6})$/i);
    if (!m) return color;
    var rgb = [0, 2, 4].map(function (i) { return parseInt(m[1].substr(i, 2), 16); });
    if ((0.299 * rgb[0] + 0.587 * rgb[1] + 0.114 * rgb[2]) / 255 < 0.8) return color;
    var bg = [34, 37, 39];
    return 'rgb(' + rgb.map(function (c, i) { return Math.round(bg[i] + (c - bg[i]) * 0.3); }).join(',') + ')';
  }

  function get(obj, path) {
    return path.split('.').reduce(function (o, k) { return o == null ? undefined : o[k]; }, obj);
  }

  // The layout keys to set in dark mode
  function darkLayout(gd) {
    var u = { 'font.color': DARK.font, 'paper_bgcolor': 'rgba(0,0,0,0)',
              'plot_bgcolor': 'rgba(0,0,0,0)', 'legend.bgcolor': 'rgba(0,0,0,0)',
              'legend.font.color': DARK.font };
    Object.keys(gd.layout).forEach(function (k) {
      if (/^[xy]axis\d*$/.test(k)) {
        u[k + '.gridcolor'] = DARK.grid;
        u[k + '.zerolinecolor'] = DARK.line;
        u[k + '.linecolor'] = DARK.line;
        // an axis given its own dark color (a second y axis, say)
        ['tickfont.color', 'title.font.color'].forEach(function (f) {
          var d = lighten(get(gd.layout[k], f));
          if (d) u[k + '.' + f] = d;
        });
      }
    });
    (gd.layout.updatemenus || []).forEach(function (m, i) {
      u['updatemenus[' + i + '].bgcolor'] = DARK.menu;
      u['updatemenus[' + i + '].bordercolor'] = DARK.menuBorder;
      u['updatemenus[' + i + '].font.color'] = DARK.font;
    });
    return u;
  }

  function themePlot(gd) {
    if (!window.Plotly || !gd.layout || !gd.data) return;
    var applied = get(gd.layout, 'font.color') === DARK.font;
    if (isDark() && !applied) {
      // Dark: keep what was there, to put it back on the way to light
      var u = darkLayout(gd), keep = {};
      Object.keys(u).forEach(function (k) {
        var v = get(gd.layout, k.replace(/\[(\d+)\]/, '.$1'));
        keep[k] = v === undefined ? null : v;
      });
      var traces = [];
      gd.data.forEach(function (t, i) {
        ['line.color', 'marker.color', 'fillcolor'].forEach(function (k) {
          var c = get(t, k), d = lighten(c);
          if (d) traces.push({ i: i, k: k, light: c, dark: d });
        });
        if (t.type === 'heatmap' && Array.isArray(t.colorscale)) {
          var scale = t.colorscale.map(function (s) { return [s[0], dim(s[1])]; });
          if (JSON.stringify(scale) !== JSON.stringify(t.colorscale))
            traces.push({ i: i, k: 'colorscale', light: t.colorscale, dark: scale });
        }
      });
      gd._uataqLight = { layout: keep, traces: traces };
      // Layout first: each redraw calls themePlot again, and from here on it
      // sees the dark font and does nothing
      Plotly.relayout(gd, u);
      restyle(gd, traces, 'dark');
    } else if (!isDark() && applied && gd._uataqLight) {
      var saved = gd._uataqLight;
      gd._uataqLight = null;
      restyle(gd, saved.traces, 'light');
      Plotly.relayout(gd, saved.layout);
    }
  }

  // One restyle per key (line, marker, fill), over all the traces it changes
  function restyle(gd, traces, which) {
    ['line.color', 'marker.color', 'fillcolor', 'colorscale'].forEach(function (k) {
      var ts = traces.filter(function (t) { return t.k === k; });
      if (!ts.length) return;
      var s = {};
      s[k] = ts.map(function (t) { return t[which]; });
      Plotly.restyle(gd, s, ts.map(function (t) { return t.i; }));
    });
  }

  function themePlots() {
    document.querySelectorAll('.js-plotly-plot').forEach(function (gd) {
      // A plot drawn again (new data in a Shiny app) comes back light
      if (!gd._uataqHooked && gd.on) {
        gd._uataqHooked = true;
        gd.on('plotly_afterplot', function () { themePlot(gd); });
      }
      themePlot(gd);
    });
  }

  // ---- leaflet map: CARTO's light tiles or its dark ones ------------------
  function themeMaps() {
    if (!window.HTMLWidgets) return;
    document.querySelectorAll('.leaflet.html-widget').forEach(function (el) {
      var w = HTMLWidgets.find('#' + el.id);
      var map = w && w.getMap && w.getMap();
      if (!map) return;
      map.eachLayer(function (layer) {
        if (layer._url && layer._url.indexOf('basemaps.cartocdn.com') >= 0) {
          var url = isDark() ? layer._url.replace('/light_all/', '/dark_all/')
                             : layer._url.replace('/dark_all/', '/light_all/');
          if (url !== layer._url) layer.setUrl(url);
        }
      });
    });
  }

  // ---- the button ---------------------------------------------------------
  function setButtons() {
    document.querySelectorAll('.theme-toggle').forEach(function (b) {
      var label = isDark() ? 'Light mode' : 'Dark mode';
      b.setAttribute('title', label);
      b.setAttribute('aria-label', label);
      var i = b.querySelector('i');
      if (i) i.className = 'fa fa-fw ' + (isDark() ? 'fa-sun-o' : 'fa-moon-o');
    });
  }

  function apply() { setButtons(); themePlots(); themeMaps(); }

  document.addEventListener('click', function (e) {
    var b = e.target.closest && e.target.closest('.theme-toggle');
    if (!b) return;
    e.preventDefault();
    var next = isDark() ? 'light' : 'dark';
    if (next === 'dark') root.setAttribute('data-theme', 'dark');
    else root.removeAttribute('data-theme');
    try { localStorage.setItem(KEY, next); } catch (err) {}
    apply();
  });

  // Plots and maps appear after the page loads (and, in Shiny, after each
  // search), so watch for new ones; a short wait groups the bursts of changes.
  var timer = null;
  function soon() { clearTimeout(timer); timer = setTimeout(apply, 150); }
  document.addEventListener('DOMContentLoaded', function () {
    apply();
    new MutationObserver(soon).observe(document.body, { childList: true, subtree: true });
  });
  window.addEventListener('load', soon);
})();
