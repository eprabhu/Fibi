// size in bytes for splitting attachment in waf enabled environment ( 1 kb = 1000 bytes, 800 kb = 800000 bytes)
export const CHUNK_SIZE = 800000;
export const HTTP_SUCCESS_STATUS = 'success';
export const HTTP_ERROR_STATUS = 'error';

export const NO_DATA_FOUND_MESSAGE = '-- No Data --';

export const CREATE_DISCLOSURE_ROUTE_URL = '/coi/create-disclosure/screening';
export const POST_CREATE_DISCLOSURE_ROUTE_URL = '/coi/disclosure/summary';
export const HOME_URL = '/coi/user-dashboard';
export const DEFAULT_UNIT_FORMAT = 'unitNumber - unitName';

export const ADMIN_DASHBOARD_RIGHTS = new Set(['MANAGE_FCOI_DISCLOSURE', 'VIEW_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE',
    'VIEW_PROJECT_DISCLOSURE', 'MANAGE_TRAVEL_DISCLOSURE', 'VIEW_TRAVEL_DISCLOSURE']);
