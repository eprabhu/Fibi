export class COI {
    coiDisclosure: CoiDisclosure;
    person: Person;
    numberOfSFI: number;
    numberOfProposal: number;
    numberOfAward: number;
    coiEntity: any;
    coiFinancialEntity: any;
    adminGroup: AdminGroup[];
    coiSections: any[];
    proposalIdlinkedInDisclosure: any;
    projectDetail: any;
    coiReviewerList: ReviewerList[];
    documentOwnerPersonId: string;
    personAttachmentsCount: Number;
    personEntitiesCount: Number;
    personNotesCount: Number;
}

export interface CoiConflictStatusType {
    conflictStatusCode: string;
    description: string;
    updateTimestamp: number;
    updateUser: string;
    isActive: boolean;
}

export interface CoiDispositionStatusType {
    dispositionStatusCode: string;
    description: string;
    updateTimestamp: number;
    updateUser: string;
    isActive: boolean;
}

export interface CoiReviewStatusType {
    reviewStatusCode: string;
    description: string;
    updateTimestamp: number;
    updateUser: string;
    isActive: boolean;
}

export interface CoiDisclosureFcoiType {
    fcoiTypeCode: string;
    description: string;
    updateTimestamp: number;
    updateUser: string;
    isActive: boolean;
}

// export interface CoiDisclosureSequence {
//     disclosureSequenceStatusCode: string;
//     description: string;
//     updateTimestamp: number;
//     updateUser: string;
//     isActive: boolean;
// }

export class CoiDisclosure {
    disclosureId: number;
    disclosureNumber: string;
    versionNumber: number;
    personId: string;
    conflictStatusCode: string;
    coiConflictStatusType: CoiConflictStatusType;
    dispositionStatusCode: string;
    coiDispositionStatusType: CoiDispositionStatusType;
    reviewStatusCode: string;
    coiReviewStatusType: CoiReviewStatusType;
    fcoiTypeCode: string;
    coiDisclosureFcoiType: CoiDisclosureFcoiType;
    versionStatus: string;
    // coiDisclosureSequence: CoiDisclosureSequence;
    certificationText?: any;
    certifiedAt?: any;
    expirationDate: number;
    certifiedBy?: any;
    createUser: string;
    createTimestamp: number;
    updateTimestamp: number;
    updateUser: string;
    isDisclosureQuestionnaire: boolean;
    person:any;
    adminPersonId: string;
    adminPersonName: string;
    adminGroupId: number;
    adminGroupName: string;
    moduleItemKey: string;
    title: string;
    numberOfSFI: any;
    riskCategoryCode: any;
    coiRiskCategory: any;
    updateUserFullName: string;
}
export interface UnitAdministratorType {
    code: string;
    description: string;
    isActive: boolean;
}

export interface UnitAdministrator {
    personId: string;
    fullName?: any;
    oldPersonId?: any;
    oldUnitAdministratorTypeCode?: any;
    unitAdministratorTypeCode: string;
    unitNumber: string;
    unitName?: any;
    updateTimestamp: any;
    updateUser: string;
    unitAdministratorType: UnitAdministratorType;
}

export interface Unit {
    unitNumber: string;
    parentUnitNumber?: any;
    organizationId: string;
    unitName: string;
    active: boolean;
    updateTimestamp: number;
    updateUser: string;
    acronym?: any;
    isFundingUnit?: any;
    unitAdministrators: UnitAdministrator[];
    unitDetail: string;
    parentUnitName?: any;
    organizationName?: any;
}

export interface Currency {
    currencyCode: string;
    currency: string;
    currencySymbol: string;
    updateUser: string;
    updateTimeStamp: number;
}

export interface CountryDetails {
    countryCode: string;
    countryName: string;
    currencyCode: string;
    currency: Currency;
    updateTimeStamp: number;
    updateUser: string;
    countryTwoCode: string;
}

export interface Person {
    personId: string;
    lastName: string;
    firstName: string;
    middleName?: any;
    fullName: string;
    priorName?: any;
    principalName: string;
    emailAddress: string;
    dateOfBirth?: any;
    age?: any;
    gender?: any;
    educationLevel?: any;
    officeLocation?: any;
    secOfficeLocation?: any;
    secOfficePhone?: any;
    school?: any;
    directoryDepartment?: any;
    countryOfCitizenshipCode?: any;
    countryOfCitizenshipDetails?: any;
    primaryTitle: string;
    directoryTitle: string;
    homeUnit: string;
    unit: Unit;
    isFaculty: boolean;
    isGraduateStudentStaff: boolean;
    isResearchStaff: boolean;
    isServiceStaff: boolean;
    isSupportStaff: boolean;
    isOtherAcadamic: boolean;
    isMedicalStaff: boolean;
    addressLine1?: any;
    addressLine2?: any;
    addressLine3?: any;
    city: string;
    country: string;
    state: string;
    postalCode: string;
    countryCode: string;
    countryDetails: CountryDetails;
    faxNumber?: any;
    pagerNumber?: any;
    mobileNumber: string;
    status: string;
    salaryAnniversary?: any;
    updateTimestamp: number;
    updateUser: string;
    supervisorPersonId?: any;
    orcidId: string;
    isWebhookActive?: any;
    dateOfInactive?: any;
    isExternalUser?: any;
    officePhone?: any;
    isPasswordChange: boolean;
    isUsernameChange: boolean;
}

export class AdminGroup {
    adminGroupId: number;
    adminGroupName: string;
    description: string;
    email: string;
    isActive: string;
    moduleCode: number;
    person: any;
    primaryPersonId: string;
    role: Role;
    roleId: number;
    updateTimestamp: number;
    updateUser: string;
}

export interface Role {
    createTimeStamp: number;
    createUser: string;
    description: string;
    roleId: number;
    roleName: string;
    roleType: RoleType;
    roleTypeCode: string;
    statusFlag: string;
    updateTimeStamp: number;
    updateUser: string;
}

export interface RoleType {
    isActive: boolean;
    roleType: string;
    roleTypeCode: string;
}

export class CommentConfiguration {
    disclosureId: any = null;
    coiReviewId: number = null;
    coiReviewCommentId: number = null;
    coiReviewActivityId = '1';
    coiSectionsTypeCode: any = null;
    modifyIndex = -1;
    comment: any = null;
    coiParentCommentId: number = null;
    isPrivate = false;
    subSectionList: any = [];
    isSubSectionComment = false;
    coiSubSectionsId: string = null;
    coiReviewCommentTag: any = [];
    coiReviewCommentAttachment: any = [];
}

export class CommentRequest {
    coiReviewId: number = null;
    coiReviewCommentId: number = null;
    coiReviewActivityId = '1';
    coiSectionsTypeCode: string = null;
    coiSectionsType: any;
    disclosureId: any = null;
    comment = '';
    coiParentCommentId: number = null;
    isPrivate = false;
    coiSubSectionsId: string = null;
    coiReviewCommentTag: any = [];
    coiReviewCommentAttachment: any = [];
}

export class RO {
    'disclosureId' = null;
    'reviewStatusCode' = '';
    'personId': any;
    'filterType': string =  'ALL';
    'currentPage': any = 1;
    'pageNumber': any = 20;
    'searchWord': any;
}

export interface getApplicableQuestionnaireData {
    applicableQuestionnaire: ApplicableQuestionnaire[]
    questionnaireId: any
    moduleItemKey: string
    moduleSubItemKey: string
    moduleItemCode: number
    moduleSubItemCode: number
    questionnaireAnswerHeaderId: any
    questionnaireAnsAttachmentId: any
    questionnaireCompleteFlag: any
    actionUserId: string
    actionPersonId: any
    actionPersonName: string
    acType: any
    questionnaireName: any
    newQuestionnaireVersion: boolean
    questionEditted: boolean
    questionnaireList: any
    questionnaireGroup: any
    header: any
    questionnaire: any
    usage: any
    fileName: any
    fileContent: any
    length: any
    remaining: any
    fileTimestamp: any
    contentType: any
    personId: any
    multipartFile: any
    moduleList: any
    isInserted: any
    updateTimestamp: any
    copyModuleItemKey: any
    questionnaireNumbers: any[]
    lookUpDetails: any
    newQuestionnaireId: any
    moduleSubItemCodes: any[]
    questionnaireBusinessRules: any
    ruleId: any
    rulePassed: any
    questionnaireMode: string
}

export interface ApplicableQuestionnaire {
    NEW_QUESTIONNAIRE_LABEL: any
    MODULE_SUB_ITEM_KEY: any
    QUESTIONNAIRE_ANS_HEADER_ID: any
    QUESTIONNAIRE_COMPLETED_FLAG: any
    MODULE_SUB_ITEM_CODE: number
    MODULE_ITEM_CODE: number
    QUESTIONNAIRE_LABEL: string
    NEW_QUESTIONNAIRE: any
    IS_NEW_VERSION: string
    VERSION_NUMBER: number
    QUESTIONNAIRE_NUMBER: number
    QUESTIONNAIRE_ID: number
    QUESTIONNAIRE: string
    ANSWERED_VERSION_NUMBER: any
    NEW_QUESTIONNAIRE_ID: any
    IS_MANDATORY: string
}

export interface ReviewerList {
  coiReviewId: number;
  assigneePersonId: string;
  disclosureId: number;
  coiDisclosure: Disclosure;
  adminGroupId: any;
  adminGroup: any;
  reviewStatusTypeCode: string;
  coiReviewStatus: CoiReviewStatus;
  description: any;
  createTimestamp: number;
  createUser: any;
  updateTimestamp: number;
  updateUser: any;
  assigneePersonName: string;
}

export interface Disclosure {
  disclosureId: number;
  personId: string;
  person: Person;
  homeUnit: string;
  unit: Unit;
  disclosureNumber: number;
  versionNumber: number;
  versionStatus: string;
  fcoiTypeCode: string;
  coiDisclosureFcoiType: CoiDisclosureFcoiType;
  conflictStatusCode: string;
  coiConflictStatusType: CoiConflictStatusType;
  dispositionStatusCode: string;
  coiDispositionStatusType: CoiDispositionStatusType;
  reviewStatusCode: string;
  coiReviewStatusType: CoiReviewStatusType;
  riskCategoryCode: string;
  coiRiskCategory: CoiRiskCategory;
  moduleCode: number;
  moduleItemKey: string;
  expirationDate: number;
  certificationText: string;
  certifiedBy: string;
  certifiedAt: number;
  revisionComment: string;
  adminGroupId: any;
  adminPersonId: string;
  updateTimestamp: number;
  updateUser: string;
  createUser: any;
  createTimestamp: number;
  updateUserFullName: any;
  createUserFullName: any;
  numberOfSFI: any;
  numberOfProposals: any;
  numberOfAwards: any;
  coiProjectTypeCode: any;
  adminGroupName: any;
  adminPersonName: any;
}
export interface CoiRiskCategory {
  riskCategoryCode: string;
  description: string;
  updateTimestamp: number;
  updateUser: string;
  isActive: boolean;
}
export interface CoiReviewStatus {
  reviewStatusCode: string;
  description: string;
  updateTimestamp: number;
  updateUser: string;
  isActive: boolean;
}

export type ModalType = 'COMPLETE' | 'START';

export interface ProjectRelationshipDetails {
    moduleCode: number;
    projectId: string;
    projectNumber: string;
    title: string;
    projectStatus: string;
    projectStartDate: number;
    projectEndDate: number;
    homeUnitNumber: string;
    homeUnitName: string;
    sponsorName: string;
    primeSponsorName: any;
    piName: string;
    keyPersonId: string;
    keyPersonName: string;
    reporterRole: string;
    conflictStatus: string;
    conflictStatusCode: string;
    entityCount: any;
    relationShipExists: boolean;
    sfiCompleted: boolean;
    disclosureStatusCount: any[];
    projectTypeCode: string;
    projectType: string;
    projectBadgeColour: string;
  }
