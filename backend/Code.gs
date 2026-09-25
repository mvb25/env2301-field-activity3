/*
  ENV2301 Field Activity 3 — Google Sheets backend

  Setup:
  1. Create a Google Sheet for the class data.
  2. Open Extensions > Apps Script and paste this file into Code.gs.
  3. In Apps Script > Project Settings > Script Properties add:
       SHEET_ID  = the Google Sheet ID
       API_TOKEN = a long random token of your choice
  4. Deploy > New deployment > Web app.
       Execute as: Me
       Access: Anyone (if allowed by your Google Workspace policy)
  5. Copy the deployed /exec URL into the Shiny environment variable DATA_API_URL.
     Put the same API_TOKEN into DATA_API_TOKEN.
*/

const SHEET_NAME = 'data';
const HEADERS = [
  'timestamp_server', 'timestamp_app', 'session_id',
  'group_id', 'patch', 'method', 'point_id', 'point_number',
  'is_repeat', 'is_reference', 'person_id', 'instrument_id',
  'temperature_c', 'rh_pct', 'wind_ms', 'canopy_pct'
];

function jsonResponse(obj) {
  return ContentService
    .createTextOutput(JSON.stringify(obj))
    .setMimeType(ContentService.MimeType.JSON);
}

function getConfig() {
  const props = PropertiesService.getScriptProperties();
  return {
    sheetId: props.getProperty('SHEET_ID'),
    token: props.getProperty('API_TOKEN')
  };
}

function checkToken(received, expected) {
  if (!expected) throw new Error('API_TOKEN is not configured in Script Properties.');
  if (!received || received !== expected) throw new Error('Invalid API token.');
}

function getDataSheet(sheetId) {
  if (!sheetId) throw new Error('SHEET_ID is not configured in Script Properties.');
  const ss = SpreadsheetApp.openById(sheetId);
  let sheet = ss.getSheetByName(SHEET_NAME);
  if (!sheet) sheet = ss.insertSheet(SHEET_NAME);

  if (sheet.getLastRow() === 0) {
    sheet.getRange(1, 1, 1, HEADERS.length).setValues([HEADERS]);
    sheet.setFrozenRows(1);
  } else {
    const existing = sheet.getRange(1, 1, 1, HEADERS.length).getValues()[0];
    const mismatch = HEADERS.some((h, i) => existing[i] !== h);
    if (mismatch) throw new Error('Header row in the data sheet does not match the expected schema.');
  }
  return sheet;
}

function doPost(e) {
  const lock = LockService.getScriptLock();
  try {
    const cfg = getConfig();
    const body = JSON.parse((e && e.postData && e.postData.contents) ? e.postData.contents : '{}');
    checkToken(body.token, cfg.token);

    lock.waitLock(10000);
    const sheet = getDataSheet(cfg.sheetId);
    const serverTime = new Date().toISOString();
    const row = HEADERS.map(h => {
      if (h === 'timestamp_server') return serverTime;
      const v = body[h];
      return (v === null || typeof v === 'undefined') ? '' : v;
    });
    sheet.appendRow(row);
    SpreadsheetApp.flush();

    return jsonResponse({status: 'ok'});
  } catch (err) {
    return jsonResponse({status: 'error', message: String(err.message || err)});
  } finally {
    try { lock.releaseLock(); } catch (e2) {}
  }
}

function doGet(e) {
  try {
    const cfg = getConfig();
    const params = (e && e.parameter) ? e.parameter : {};
    checkToken(params.token, cfg.token);

    if ((params.action || 'read') !== 'read') {
      return jsonResponse({status: 'error', message: 'Unknown action.'});
    }

    const sheet = getDataSheet(cfg.sheetId);
    const lastRow = sheet.getLastRow();
    if (lastRow <= 1) return jsonResponse({status: 'ok', rows: []});

    const values = sheet.getRange(2, 1, lastRow - 1, HEADERS.length).getValues();
    const rows = values.map(row => {
      const obj = {};
      HEADERS.forEach((h, i) => obj[h] = row[i]);
      return obj;
    });

    return jsonResponse({status: 'ok', rows: rows});
  } catch (err) {
    return jsonResponse({status: 'error', message: String(err.message || err)});
  }
}
