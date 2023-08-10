import { DEFAULT_ENDPOINT_FETCH_LIMIT, ENDPOINT_SPONSOR_OUTPUT_FORMAT, LEAD_UNIT_OUTPUT_FORMAT } from '../../app-constants';

const endPointOptions = {
    contextField: '',
    formatString: '',
    path: '',
    defaultValue: '',
    params: {},
    filterFields: ''
};

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForSponsor(
    { contextField = ENDPOINT_SPONSOR_OUTPUT_FORMAT, formatString = ENDPOINT_SPONSOR_OUTPUT_FORMAT, defaultValue = '', params = {}} = {}
) {
    endPointOptions.contextField = contextField;
    endPointOptions.formatString = formatString;
    endPointOptions.path = 'findSponsors';
    endPointOptions.defaultValue = defaultValue;
    endPointOptions.params = getParams(params);
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
        defaultValue = '', rootGroupId = '', params = {}} = {}
) {
    endPointOptions.contextField = contextField;
    endPointOptions.formatString = formatString;
    endPointOptions.path = `sponsorHierarchy/sponsors/`;
    endPointOptions.defaultValue = defaultValue;
    endPointOptions.params = getParams(params);
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForSponsorByType(sponsorName, sponsorTypeCode, params = {}) {
    endPointOptions.contextField = ENDPOINT_SPONSOR_OUTPUT_FORMAT;
    endPointOptions.formatString = ENDPOINT_SPONSOR_OUTPUT_FORMAT;
    endPointOptions.path = 'fetchSponsorsBySponsorType';
    endPointOptions.defaultValue = sponsorName;
    endPointOptions.params = getParams({...sponsorTypeCode, ...params});
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForLeadUnit(defaultValue = '', baseUrl = '', formatString = 'unitName') {
    endPointOptions.contextField = formatString;
    endPointOptions.formatString = formatString;
    // endPointOptions.contextField = LEAD_UNIT_OUTPUT_FORMAT;
    // endPointOptions.formatString = LEAD_UNIT_OUTPUT_FORMAT;
    endPointOptions.path = baseUrl + '/' + 'findLeadUnits';
    endPointOptions.defaultValue = defaultValue;
    endPointOptions.params = null;
    endPointOptions.filterFields = 'unitName, unitNumber';
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForSchoolUnit( params = {}, defaultValue = '') {
    endPointOptions.contextField = LEAD_UNIT_OUTPUT_FORMAT;
    endPointOptions.formatString = LEAD_UNIT_OUTPUT_FORMAT;
    endPointOptions.path = 'getSchoolUnitDetails';
    endPointOptions.defaultValue = defaultValue;
    endPointOptions.params = getParams(params);
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

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForOrganization(params = {}) {
    endPointOptions.contextField = 'organizationName';
    endPointOptions.formatString = 'organizationName';
    endPointOptions.path = 'findOrganizations';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForCountry(params = {}) {
    endPointOptions.contextField = 'countryName';
    endPointOptions.formatString = 'countryName';
    endPointOptions.path = 'findCountry';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForCongressionalDistrict(params = {}) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findCongressionalDistricts';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForRolodexSearch(params = {}) {
    endPointOptions.contextField = 'fullName';
    endPointOptions.formatString = 'fullName';
    endPointOptions.path = 'findRolodex';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
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

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForCostElements(path, params = {}) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'costElement - description';
    endPointOptions.path = path;
    endPointOptions.defaultValue = '';   
    endPointOptions.params = getParams(params);       
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

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForProfitCentre(params = {}) {
    endPointOptions.contextField = 'profitCenterCode';
    endPointOptions.formatString = 'profitCenterDetails';
    endPointOptions.path = 'findProfitCenter';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);   
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForFundCentre(params = {}) {
    endPointOptions.contextField = 'fundCenterCode';
    endPointOptions.formatString = 'fundCenterCode - description';
    endPointOptions.path =  'findFundCenter';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);   
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForCostCentre(params = {}) {
    endPointOptions.contextField = 'costCenterCode';
    endPointOptions.formatString = 'costCenterDetails';
    endPointOptions.path = 'findCostCenter';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);   
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForGrandCode(params = {}) {
    endPointOptions.contextField = 'grantCode';
    endPointOptions.formatString = 'grantDetails';
    endPointOptions.path = 'findGrantCode';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForKeyWords(params = {}) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findKeyWords';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForAffiliationKeyWords(params = {}) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findAffiliationInstitution';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForExtReviewerKeyWords(params = {}) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findSpecialismKeywords';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForAwardNumber(params = {}) {
    endPointOptions.contextField = 'awardNumber';
    endPointOptions.formatString =
    ' awardNumber | accountNumber | title | sponsorName | sponsorAwardNumber | unitName | principalInvestigator ';
    endPointOptions.path =  'findAward';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);  
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForScopus(params = {}) {
    endPointOptions.contextField =  'title';
    endPointOptions.formatString = 'scopusId | creator | title';
    endPointOptions.path =  'findScopus';
    endPointOptions.defaultValue = '';    
    endPointOptions.params = getParams(params);
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForArea(params = {}) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findResearchTypeArea';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForSubArea(params = {}) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path = 'findResearchTypeSubArea';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
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

/**
 * @param params will have fetchLimit as one of the values 
 * to specify limit of data to fetched,
 * it should be given inside params as {'fetchLimit' : requiredLimit}
 * requiredLimit can be either null or any valid number.
 * if no limit is specified default fetch limit 50 will be used.
 * if limit is null then full list will return, this may cause performance issue.
 */
export function getEndPointOptionsForTraining( params = {}) {
    endPointOptions.contextField = 'description';
    endPointOptions.formatString = 'description';
    endPointOptions.path =  'loadTrainingList';
    endPointOptions.defaultValue = '';
    endPointOptions.params = getParams(params);
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

export function getEndPointOptionsForMappedClaimTemplate() {
    endPointOptions.contextField = 'claimTemplateCode';
    endPointOptions.formatString = 'claimTemplateCode - description';
    endPointOptions.path =  'mappedClaimTemplates';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForClaimTemplate() {
    endPointOptions.contextField = 'claimTemplateCode';
    endPointOptions.formatString = 'claimTemplateCode - description';
    endPointOptions.path =  'claimTemplates';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

/**this function will return params after
 * setting fetchLimit, if no fetchLimit is given 
 * in params, then DEFAULT_ENDPOINT_FETCH_LIMIT 
 * will set, otherwise passed value will be used.
 **/
export function getParams(params) {
    if (params && !params.hasOwnProperty('fetchLimit')) {
        params['fetchLimit'] = DEFAULT_ENDPOINT_FETCH_LIMIT;
    }
    return params;
}

export function getEndPointOptionsForPosititonId() {
    endPointOptions.contextField = 'positionId';
    endPointOptions.formatString = 'positionId';
    endPointOptions.path =  'findPositionId';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForEntity(baseUrl= '') {
    endPointOptions.contextField = 'entityName';
    endPointOptions.formatString = 'entityName';
    endPointOptions.path = baseUrl + '/' + 'searchEntity';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForProposalDisclosure(baseUrl = '') {
    endPointOptions.contextField = 'title';
    endPointOptions.formatString = '#moduleItemId - title';
    endPointOptions.path = baseUrl + '/' + 'loadProposalsForDisclosure';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}

export function getEndPointOptionsForCoiAwardNumber (baseUrl = '') {
    endPointOptions.contextField = 'title';
    endPointOptions.formatString =
        'moduleItemKey | accountNumber | title | sponsorName | sponsorAwardNumber | unitName | PrincipalInvestigator';
    endPointOptions.path = baseUrl + '/' + 'loadAwardsForDisclosure';
    endPointOptions.defaultValue = '';
    endPointOptions.params = null;
    return JSON.parse(JSON.stringify(endPointOptions));
}