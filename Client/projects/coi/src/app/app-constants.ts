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
export const CONSULTING_REDIRECT_URL = '/coi/consulting/form';


export const ELASTIC_FIBI_PERSON_OUTPUT_FORMAT = 'full_name | prncpl_nm';
export const ELASTIC_ENTITY_FORMAT = 'entity_name | country_name | entity_type';
export const ELASTIC_AWARD_OUTPUT_FORMAT = ' award_number | account_number | title | sponsor | lead_unit_name | pi_name | grant_call_name';
export const ELASTIC_ROLODEX_PERSON_OUTPUT_FORMAT = 'rolodex_id | full_name | organization | email_address';
export const ELASTIC_ORGANIZATION_OUTPUT_FORMAT = 'rolodex_id | full_name | organization | email_address | address';
export const ELASTIC_IP_OUTPUT_FORMAT = 'proposal_number | title | sponsor | lead_unit_name | activity_type | proposal_type | pi_full_name | status';
export const ELASTIC_PROPOSAL_OUTPUT_FORMAT = 'proposal_id | title | full_name | category | type | status | sponsor | lead_unit_name';
export const ELASTIC_GRANT_OUTPUT_FORMAT = 'grant_header_id | title | grant_type | sponsor | funding_scheme | status';
export const ELASTIC_COI_OUTPUT_FORMAT = 'coi_disclosure_number | full_name | disclosure_disposition |disclosure_status | module_item_key';
export const ELASTIC_IACUC_OUTPUT_FORMAT = 'protocol_number | title | lead_unit_name | status | person_name | protocol_type';
export const ELASTIC_IRB_OUTPUT_FORMAT = 'protocol_number | title | lead_unit_name | status | person_name';
export const ELASTIC_AGREEMENT_OUTPUT_FORMAT = 'agreement_request_id | title | agreement_type | unit_name | agreement_status | principal_person_full_name | aa_person_full_name | sponsor_name | requestor_full_name';
export const ELASTIC_EXTERNAL_REVIEWER_OUTPUT_FORMAT = 'full_name | email_addr | Academic Rank : academic_rank | H-Index: hindex';

export const ADMIN_DASHBOARD_RIGHTS = ['MANAGE_FCOI_DISCLOSURE', 'VIEW_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE', 'VIEW_PROJECT_DISCLOSURE',
    'MANAGE_TRAVEL_DISCLOSURE', 'VIEW_TRAVEL_DISCLOSURE', 'MANAGE_CONSULTING_DISCLOSURE', 'VIEW_CONSULTING_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE_OVERVIEW'];

export const COI_CONFIGURATIONS_RIGHTS = [
    'APPLICATION_ADMINISTRATOR',
    'MAINTAIN_QUESTIONNAIRE',
    'MAINTAIN_USER_ROLES',
    'MAINTAIN_ROLE',
    'MAINTAIN_PERSON',
    'MAINTAIN_TRAINING',
    'VIEW_KEY_PERSON_TIMESHEET',
    'MAINTAIN_KEY_PERSON_TIMESHEET',
    'MAINTAIN_DELEGATION',
    'MAINTAIN_ORCID_WORKS'
];

export const COI_DISCLOSURE_SUPER_ADMIN_RIGHTS = ['COI_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_COI'];

export const PROJECT_OVERVIEW_RIGHTS = ['MANAGE_PROJECT_DISCLOSURE_OVERVIEW'];
export const TRAVEL_DISCLOSURE_RIGHTS = ['MANAGE_TRAVEL_DISCLOSURE', 'VIEW_TRAVEL_DISCLOSURE'];
export const CONSULTING_DISCLOSURE_RIGHTS = ['MANAGE_CONSULTING_DISCLOSURE', 'VIEW_CONSULTING_DISCLOSURE'];

export const FCOI_DISCLOSURE_RIGHTS = ['MANAGE_FCOI_DISCLOSURE', 'VIEW_FCOI_DISCLOSURE'];
export const PROJECT_DISCLOSURE_RIGHTS = ['MANAGE_PROJECT_DISCLOSURE', 'VIEW_PROJECT_DISCLOSURE'];
export const FCOI_PROJECT_DISCLOSURE_RIGHTS = ['MANAGE_FCOI_DISCLOSURE', 'VIEW_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE', 'VIEW_PROJECT_DISCLOSURE'];

export const OPA_DISCLOSURE_RIGHTS = ['MANAGE_OPA_DISCLOSURE', 'VIEW_OPA_DISCLOSURE'];
export const OPA_DISCLOSURE_ADMIN_RIGHTS = ['OPA_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_OPA'];

export const ENTITY_RIGHTS = ['MANAGE_ENTITY', 'VIEW_ENTITY', 'MANAGE_ENTITY_SPONSOR', 'MANAGE_ENTITY_ORGANIZATION', 'MANAGE_ENTITY_COMPLIANCE', 'VERIFY_ENTITY'];
export const ENTITY_SPONSOR_RIGHT = ['MANAGE_ENTITY_SPONSOR'];
export const ENTITY_ORGANIZATION_RIGHT = ['MANAGE_ENTITY_ORGANIZATION'];
export const ENTITY_COMPLIANCE_RIGHT = ['MANAGE_ENTITY_COMPLIANCE'];

export const SFI_ADDITIONAL_DETAILS_SECTION_NAME = 'SFI Details';

export const DATE_PLACEHOLDER = 'MM/dd/YYYY';
export const DEFAULT_DATE_FORMAT = 'MM/dd/yyyy';
export const LONG_DATE_FORMAT = 'MM/dd/yyyy h:mm:ss a';
export const TIME_FORMAT = 'h:mm:ss a';

//  Angular Material date picker
export const DATE_PICKER_FORMAT_MATERIAL = {
    parse: {
        dateInput: 'MM/DD/YYYY HH:mm:ss',
    },
    display: {
        dateInput: 'MM/DD/YYYY',
        monthYearLabel: 'MMM YYYY',
        dateA11yLabel: 'LL',
        monthYearA11yLabel: 'MMMM YYYY',
    },
};

export const EDITOR_CONFIGURATION = {
    link: {
        addTargetToExternalLinks: true,
        defaultProtocol: 'http://'
    },
    removePlugins: ['imageUpload', 'mediaEmbed'],
    toolbar: {
        removeItems: ['imageUpload', 'mediaEmbed', 'uploadImage']
    },
    image: {},
    mediaEmbed: {},
};

export const COI_MODULE_CODE = 8;
export const OPA_MODULE_CODE = 23;
export const TRAVEL_MODULE_CODE = 24;
export const CONSULTING_MODULE_CODE = 27;
export const GLOBAL_ENTITY_MODULE_CODE = 26;
export const GLOBAL_ENTITY_SPONSOR_SUBMODULE_CODE = 1;
export const GLOBAL_ENTITY_ORGANIZATION_SUBMODULE_CODE = 2;
export const GLOBAL_ENTITY_COMPLIANCE_SUBMODULE_CODE = 3;
// Proposal Certification Questionaire Sub Module Code
export const EXTERNAL_QUESTIONAIRE_MODULE_SUB_ITEM_CODE = 802;

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

export const CONSULTING_REVIEW_STATUS = {
    PENDING: '1',
    RETURNED: '8',
    WITHDRAWN: '7'
};

// sso timeout related variables
export const SSO_TIMEOUT_ERROR_MESSAGE = 'Your session has been expired.';
export const SSO_TIMEOUT_ERROR_CODE = 0;
export const SSO_LOGOUT_URL = '';

export const AWARD_EXTERNAL_RESOURCE_URL = '#/fibi/award/overview?awardId={projectId}';
export const PROPOSAL_EXTERNAL_RESOURCE_URL = '#/fibi/proposal/overview?proposalId={projectId}';
export const IP_EXTERNAL_RESOURCE_URL = '#/fibi/instituteproposal/overview?instituteProposalId={projectId}';

export const URL_FOR_DISCLOSURE_PROJECT = '/fcoiDisclosure/projects/{disclosureId}';

export const CLASS_RED_BADGE = 'red-badge';
export const CLASS_GREEN_BADGE = 'green-badge';
export const CLASS_BROWN_BADGE = 'brown-badge';

export const PROJECT_CONFLICT_STATUS_BADGE: { [key: string]: string } = {
    '100': CLASS_GREEN_BADGE,
    '200': CLASS_BROWN_BADGE,
    '300': CLASS_RED_BADGE,
    '400': CLASS_GREEN_BADGE
};

export const DISCLOSURE_CONFLICT_STATUS_BADGE: { [key: string]: string } = {
    '1': CLASS_GREEN_BADGE,
    '2': CLASS_BROWN_BADGE,
    '3': CLASS_RED_BADGE,
    '4': CLASS_GREEN_BADGE,
    '5': CLASS_BROWN_BADGE,
    '6': CLASS_RED_BADGE,
};

export const COMMON_ERROR_TOAST_MSG = 'Something went wrong, please try again.';

export const ENTITY_DOCUMNET_STATUS_TYPE = {
    DUPLICATE: '3',
    ACTIVE: '1',
    INACTIVE: '2'
};

export const ENTITY_VERIFICATION_STATUS = {
    VERIFIED: '1',
    UNVERIFIED: '2'
};

export const DISCLOSURE_TYPE = {
    INITIAL: '1',
    PROJECT: '2',
    REVISION: '3'
};

export const PROJECT_TYPE = {
    AWARD: '1',
    IP: '2',
    DEV_PROPOSAL: '3'
};

export const FEED_STATUS_CODE = {
    READY_TO_FEED: '2',
    SUCCESS: '3',
    ERROR: '4',
    NOT_READY_TO_FEED: '1'
};

export const AWARD_DETAILS_ORDER_WITHOUT_ROLE = [
    'PI',
    'LEAD_UNIT',
    'PROJECT_STATUS',
    'ACCOUNT_NUMBER',
    'SPONSOR',
    'PRIME_SPONSOR',
    'PERIOD'
];

export const PROPOSAL_DETAILS_ORDER_WITHOUT_ROLE = [
    'PI',
    'LEAD_UNIT',
    'PROJECT_STATUS',
    'SPONSOR',
    'PRIME_SPONSOR',
    'PERIOD'
];

export const PROPOSAL_DETAILS_ORDER = [
    'PI',
    'REPORTER_ROLE',
    'LEAD_UNIT',
    'PROJECT_STATUS',
    'SPONSOR',
    'PRIME_SPONSOR',
    'PERIOD'
];

export const AWARD_DETAILS_ORDER = [
    'PI',
    'REPORTER_ROLE',
    'LEAD_UNIT',
    'PROJECT_STATUS',
    'ACCOUNT_NUMBER',
    'SPONSOR',
    'PRIME_SPONSOR',
    'PERIOD'
];

export const PROJECT_DETAILS_ORDER = {
    1: AWARD_DETAILS_ORDER,
    2: PROPOSAL_DETAILS_ORDER,
    3: PROPOSAL_DETAILS_ORDER,
    undefined: PROPOSAL_DETAILS_ORDER,
    null: PROPOSAL_DETAILS_ORDER,
};

export const PROJECT_DETAILS_ORDER_WITHOUT_ROLE = {
    1: AWARD_DETAILS_ORDER_WITHOUT_ROLE,
    2: PROPOSAL_DETAILS_ORDER_WITHOUT_ROLE,
    3: PROPOSAL_DETAILS_ORDER_WITHOUT_ROLE,
    undefined: PROPOSAL_DETAILS_ORDER_WITHOUT_ROLE,
    null: PROPOSAL_DETAILS_ORDER_WITHOUT_ROLE,
};
