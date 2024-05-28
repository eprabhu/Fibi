// size in bytes for splitting attachment in waf enabled environment ( 1 kb = 1000 bytes, 800 kb = 800000 bytes)
export const CHUNK_SIZE = 800000;
export const HTTP_SUCCESS_STATUS = 'success';
export const HTTP_ERROR_STATUS = 'error';

export const NO_DATA_FOUND_MESSAGE = '--';

export const CREATE_DISCLOSURE_ROUTE_URL = '/coi/create-disclosure/screening';
export const POST_CREATE_DISCLOSURE_ROUTE_URL = '/coi/disclosure/summary';
export const CREATE_TRAVEL_DISCLOSURE_ROUTE_URL = '/coi/create-travel-disclosure/travel-details';
export const POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL = '/coi/travel-disclosure/summary';
export const REPORTER_HOME_URL = '/coi/user-dashboard';
export const ADMIN_DASHBOARD_URL = '/coi/admin-dashboard';
export const DEFAULT_UNIT_FORMAT = 'unitNumber - unitName';
export const OPA_REDIRECT_URL = '/coi/opa/form';

export const ELASTIC_FIBI_PERSON_OUTPUT_FORMAT = 'full_name | prncpl_nm';
export const ELASTIC_ENTITY_FORMAT = 'entity_name | country_name | entity_type';

export const ADMIN_DASHBOARD_RIGHTS = new Set(['MANAGE_FCOI_DISCLOSURE', 'VIEW_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE',
    'VIEW_PROJECT_DISCLOSURE', 'MANAGE_TRAVEL_DISCLOSURE', 'VIEW_TRAVEL_DISCLOSURE']);

export const OPA_DASHBOARD_RIGHTS = new Set(['MANAGE_OPA_DISCLOSURE', 'VIEW_OPA_DISCLOSURE']);

export const SFI_ADDITIONAL_DETAILS_SECTION_NAME = 'SFI Details';

export const DATE_PLACEHOLDER = 'MM/dd/YYYY';

export const EDITOR_CONFIGURATION = {
    link: {
      addTargetToExternalLinks: true,
      defaultProtocol: 'http://'
    },
    removePlugins: ['imageUpload', 'mediaEmbed'],
    toolbar: {
      removeItems: [ 'imageUpload', 'mediaEmbed', 'uploadImage' ]
    },
    image: {},
    mediaEmbed: {},
  };

export const COI_MODULE_CODE = 8;
export const OPA_MODULE_CODE = 23;
export const TRAVEL_MODULE_CODE = 24;

export const COI_REVIEW_STATUS_TYPE = {
    SUBMITTED: '2'
};

export const COI_CONFLICT_STATUS_TYPE = {
    NO_CONFLICT_WITHOUT_SFI: '4'
};

export const TRAVEL_REVIEW_STATUS = {
    SUBMITTED: '2',
    REVIEW_IN_PROGRESS: '3',
    APPROVED_ACKNOWLEDGED: '7'
};

export const OPA_REVIEW_STATUS = {
    PENDING: '1',
    RETURNED: '5',
    WITHDRAWN: '6'
};

// sso timeout related variables
export const SSO_TIMEOUT_ERROR_MESSAGE = 'Your session has been expired.';
export const SSO_TIMEOUT_ERROR_CODE = 0;
export const SSO_LOGOUT_URL = '';
