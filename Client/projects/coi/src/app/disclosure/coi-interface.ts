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
}

export class CoiTravelDisclosure {
    travelDisclosureId: number;
    travelNumber: number;
    versionNumber: number;
    versionStatus = 'ACTIVE';
    personId:string
    entityId: number;
    entityNumber: number;
    travelStatusCode: string;
    isSponsoredTravel: boolean;
    travelTitle: string;
    purposeOfTheTrip: string;
    travelAmount: number;
    travelStartDate: any;
    noOfDays: number;
    destinationCity: string;
    destinationCountry: string;
    relationshipToYourResearch: string;
    travelState: string;
    travelEndDate: any;
    acknowledgeBy: string;
    acknowledgeAt: string;
    travelTravellerId: string;
    travellerTypeCode: Array<string> = [];
    isInternationalTravel: boolean;
}

export interface TravelDisclosureTraveller {
    description: string;
    isActive: boolean;
    isChecked?: boolean;
    travelerTypeCode?: string;
    travelStatusCode?: string;
    updateTimestamp: number;
    updateUser: string;
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

export class SFIDashboardRequestObject {
    advancedSearch = 'L';
    pageNumber = 30;
    sort = {};
    isDownload = false;
    property16 = '';
    property8 = '';
    property17 = null;
    property18 = null;
    property19 = null;
}
