import { ENDPOINT_SPONSOR_OUTPUT_FORMAT, LEAD_UNIT_OUTPUT_FORMAT } from '../../app-constants';

const endPointOptions = {
    contextField: '',
    formatString: '',
    path: '',
    defaultValue: '',
    params: '',
    filterFields: ''
};


export function getEndPointOptionsForSponsor(
    { contextField = ENDPOINT_SPONSOR_OUTPUT_FORMAT, formatString = ENDPOINT_SPONSOR_OUTPUT_FORMAT, defaultValue = '', params = null } = {}
) {
    endPointOptions.contextField = contextField;
    endPointOptions.formatString = formatString;
    endPointOptions.path = 'findSponsors';
    endPointOptions.defaultValue = defaultValue;
    endPointOptions.params = params;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForSponsorHierarchy(defaultValue = '', params = null) {
    endPointOptions.contextField = 'sponsorGroupName';
    endPointOptions.formatString = 'sponsorGroupName' ;
    endPointOptions.path =  'sponsorHierarchy/sponsorGroups';
    endPointOptions.defaultValue = defaultValue;
    endPointOptions.params = params;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForSponsorGroup(
    { contextField = ENDPOINT_SPONSOR_OUTPUT_FORMAT, formatString = ENDPOINT_SPONSOR_OUTPUT_FORMAT,
        defaultValue = '', rootGroupId = '' } = {}
) {
    endPointOptions.contextField = contextField;
    endPointOptions.formatString = formatString;
    endPointOptions.path = `sponsorHierarchy/${rootGroupId}/sponsors/`;
    endPointOptions.defaultValue = defaultValue;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForSponsorByType(sponsorName, sponsorTypeCode) {
    endPointOptions.contextField = ENDPOINT_SPONSOR_OUTPUT_FORMAT;
    endPointOptions.formatString = ENDPOINT_SPONSOR_OUTPUT_FORMAT;
    endPointOptions.path = 'fetchSponsorsBySponsorType';
    endPointOptions.defaultValue = sponsorName;
    endPointOptions.params = sponsorTypeCode ? sponsorTypeCode : null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForLeadUnit(defaultValue = '') {
    endPointOptions.contextField = LEAD_UNIT_OUTPUT_FORMAT;
    endPointOptions.formatString = LEAD_UNIT_OUTPUT_FORMAT;
    endPointOptions.path = 'findLeadUnits';
    endPointOptions.defaultValue = defaultValue;
    endPointOptions.params = null;
    endPointOptions.filterFields = 'unitName, unitNumber';
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForGrantCall() {
    endPointOptions.contextField = 'grantCallName';
    endPointOptions.formatString = 'grantCallName';
    endPointOptions.path = 'findGrantCall';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForOrganization() {
    endPointOptions.contextField = 'organizationName';
    endPointOptions.formatString = 'organizationName';
    endPointOptions.path = 'findOrganizations';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForCountry() {
    endPointOptions.contextField = 'countryName';
    endPointOptions.formatString = 'countryName';
    endPointOptions.path = 'findCountry';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}
export function getEndPointOptionsForEntity() {
    endPointOptions.contextField = 'coiEntityName';
    endPointOptions.formatString = 'coiEntityName';
    endPointOptions.path = 'searchEntity';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}
export function getEndPointOptionsForProposalDisclosure() {
    endPointOptions.contextField = 'title';
    endPointOptions.formatString = '#moduleItemId - title';
    endPointOptions.path = 'loadProposalsForDisclosure';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}
export function getEndPointOptionsForCongressionalDistrict() {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findCongressionalDistricts';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}
export function getEndPointOptionsForRolodexSearch() {
    endPointOptions.contextField = 'fullName';
    endPointOptions.formatString = 'fullName';
    endPointOptions.path = 'findRolodex';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}
export function getEndPointOptionsForDepartment() {
    endPointOptions.contextField = LEAD_UNIT_OUTPUT_FORMAT;
    endPointOptions.formatString = LEAD_UNIT_OUTPUT_FORMAT;
    endPointOptions.path = 'findDepartment';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    endPointOptions.filterFields = 'unitName, unitNumber';
    return JSON.parse(JSON.stringify(endPointOptions));
}
export function getEndPointOptionsForCostElements(path, params) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'costElement - description';
    endPointOptions.path = path;
    endPointOptions.defaultValue = '';
    endPointOptions.params = params;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForManpower(contextField, formatString, path, params) {
    endPointOptions.contextField = contextField;
    endPointOptions.formatString = formatString;
    endPointOptions.path = path;
    endPointOptions.defaultValue = '';
    endPointOptions.params = params;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForProfitCentre() {
    endPointOptions.contextField = 'profitCenterCode';
    endPointOptions.formatString = 'profitCenterDetails';
    endPointOptions.path = 'findProfitCenter';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForFundCentre() {
    endPointOptions.contextField = 'fundCenterCode';
    endPointOptions.formatString = 'fundCenterCode - description';
    endPointOptions.path =  'findFundCenter';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForCostCentre() {
    endPointOptions.contextField = 'costCenterCode';
    endPointOptions.formatString = 'costCenterDetails';
    endPointOptions.path = 'findCostCenter';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForGrandCode() {
    endPointOptions.contextField = 'grantCode';
    endPointOptions.formatString = 'grantDetails';
    endPointOptions.path = 'findGrantCode';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForKeyWords() {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findKeyWords';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}
export function getEndPointOptionsForExtReviewerKeyWords() {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findSpecialismKeywords';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForAwardNumber() {
    endPointOptions.contextField = 'awardNumber';
    endPointOptions.formatString =
    ' awardNumber | accountNumber | title | sponsorName | sponsorAwardNumber | unitName | principalInvestigator ';
    endPointOptions.path =  'findAward';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForScopus() {
    endPointOptions.contextField =  'title';
    endPointOptions.formatString = 'scopusId | creator | title';
    endPointOptions.path =  'findScopus';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForArea(param = null) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findResearchTypeArea';
    endPointOptions.defaultValue = '';
    endPointOptions.params = param;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForSubArea(param = null) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findResearchTypeSubArea';
    endPointOptions.defaultValue = '';
    endPointOptions.params = param;
  return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForRole() {
    endPointOptions.contextField = 'roleName';
    endPointOptions.formatString = 'roleName';
    endPointOptions.path =  'findRole';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForTraining() {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path =  'loadTrainingList';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForLetterTemplatetypes() {
    endPointOptions.contextField = 'letterTemplateTypeCode';
    endPointOptions.formatString = 'letterTemplateTypeCode - fileName';
    endPointOptions.path =  'letterTemplate';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForLetterTemplateMapping() {
    endPointOptions.contextField = 'templateCode';
    endPointOptions.formatString = 'templateCode - templateDescription';
    endPointOptions.path =  'letterTemplateMapping';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}
