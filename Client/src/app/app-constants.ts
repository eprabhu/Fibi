/**
 * Format strings of different Elastic configurations
 */
export const ELASTIC_FIBI_PERSON_OUTPUT_FORMAT = 'full_name | prncpl_nm';
export const ELASTIC_AWARD_OUTPUT_FORMAT = ' award_number | account_number | title | sponsor | lead_unit_name | pi_name | grant_call_name';
export const ELASTIC_ROLODEX_PERSON_OUTPUT_FORMAT = 'rolodex_id | full_name | email_address';
export const ELASTIC_ORGANIZATION_OUTPUT_FORMAT = 'rolodex_id | full_name | organization | email_address | address';
export const ELASTIC_IP_OUTPUT_FORMAT = 'proposal_number | title | sponsor | lead_unit_name | activity_type | proposal_type | pi_full_name | status';
export const ELASTIC_PROPOSAL_OUTPUT_FORMAT = 'proposal_id | title | full_name | category | type | status | sponsor | lead_unit_name';
export const ELASTIC_GRANT_OUTPUT_FORMAT = 'grant_header_id | title | grant_type | sponsor | funding_scheme | status';
export const ELASTIC_COI_OUTPUT_FORMAT = 'disclosure_id | full_name | category | status';
export const ELASTIC_IACUC_OUTPUT_FORMAT = 'protocol_number | title | lead_unit_name | status | person_name | protocol_type';
export const ELASTIC_IRB_OUTPUT_FORMAT = 'protocol_number | title | lead_unit_name | status | person_name';
export const ELASTIC_AGREEMENT_OUTPUT_FORMAT =
  'agreement_request_id | title | unit_name | agreement_type | agreement_status | principal_person_full_name| aa_person_full_name | requestor_full_name';
export const ELASTIC_EXTERNAL_REVIEWER_OUTPUT_FORMAT = 'full_name | prncpl_nm';

export const HTTP_ERROR_STATUS = 'error';
export const HTTP_SUCCESS_STATUS = 'success';

// sso timeout related variables
export const SSO_TIMEOUT_ERROR_MESSAGE = 'Your session has been expired.';
export const SSO_TIMEOUT_ERROR_CODE = 0;
export const SSO_LOGOUT_URL = '';

/* KKI Specific Change (Don't Delete) in Month/Day/Year format for DEFAULT_DATE_FORMAT, LONG_DATE_FORMAT,
parseInput, fullPickerInput, datePickerInput */

export const DEFAULT_DATE_FORMAT = 'dd/MM/yyyy';
export const LONG_DATE_FORMAT = 'dd/MM/yyyy h:mm:ss a';
export const TIME_FORMAT = 'h:mm:ss a';

export const AWARD_LABEL = 'Award';
export const KEY_PERSON_LABEL = 'Key Personnel';
export const COMMON_APPROVE_LABEL = 'Approve';
export const COMMON_RETURN_LABEL = 'Return';
export const COMMON_PERIODS_AND_TOTAL_LABEL = 'Budget Periods & Total';

export const AREA_OF_RESEARCH = 'Area of Research/Societal Challenge Area';
export const DATE_PICKER_FORMAT = {
  parseInput: 'DD/MM/YYYY HH:mm:ss',
  fullPickerInput: 'DD/MM/YYYY HH:mm:ss',
  datePickerInput: 'DD/MM/YYYY',
  timePickerInput: 'HH:mm:ss',
  monthYearLabel: 'MMM YYYY',
  dateA11yLabel: 'LL',
  monthYearA11yLabel: 'MMMM YYYY',
};
// size in bytes for splitting attachment in waf enabled environment ( 1 kb = 1000 bytes, 800 kb = 800000 bytes)
export const CHUNK_SIZE = 800000;

export const NUMBER_FORMAT = '1.0';
// For editor configuration
export const EDITOR_CONFIURATION = {
  link: {
    addTargetToExternalLinks: true,
    defaultProtocol: 'http://'
  },
  removePlugins: ['MediaEmbed', 'ImageUpload'],
  image: {},
  mediaEmbed: {},
};

export const DEFAULT_SPONSOR = {
  sponsorCode: '000100',
  sponsorName: 'Air-Force'
};


// Redirection link for external user registration application
export const EXT_USER_REGN_LINK = '';

/**
 * Format strings of different Endpoint configurations
 */
export const ENDPOINT_SPONSOR_OUTPUT_FORMAT = 'sponsorCode - sponsorName (acronym)';
export const LEAD_UNIT_OUTPUT_FORMAT = 'unitNumber - unitName';
export const COMPLIANCE_URL = '';
export const ROLODEX_FULL_NAME = 'LASTNAME, FIRSTNAME MIDDLENAME';

//  Angular Material date picker

export const DATE_PICKER_FORMAT_MATERIAL = {
  parse: {
    dateInput: 'DD/MM/YYYY HH:mm:ss',
  },
  display: {
    dateInput: 'DD/MM/YYYY',
    monthYearLabel: 'MMM YYYY',
    dateA11yLabel: 'LL',
    monthYearA11yLabel: 'MMMM YYYY',
  },
}; 
