export type ModalSize = 'sm' | 'lg' | 'xl' | '';
export type TabType = 'TRAVEL_DETAILS' | 'CERTIFY' | 'HISTORY_CREATE' | 'HISTORY_VIEW' | 'SUMMARY' | 'RELATED_DISCLOSURES';

export class CoiTravelDisclosure {
    travellerTypeCode: Array<string> = [];
    entityId: number;
    entityNumber: number;
    travelTitle: string;
    travelState: string;
    destinationCity: string;
    destinationCountry: string;
    purposeOfTheTrip: string;
    relationshipToYourResearch: string;
    travelAmount: number;
    travelStartDate: any;
    travelEndDate: any;
    isInternationalTravel: boolean;
    isSponsoredTravel: boolean;
    personId: string;
    noOfDays: number;
    homeUnit: string | null;
    description: string;
    travelDisclosureId?: number;
    travelEntityName?: string;
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

export class TravelCreateModalDetails {
    homeUnit: string | null;
    description: string | null;
    personId: string | null;
    homeUnitName: string | null;
}

export class TravelDisclosure {
    travelDisclosureId: number;
    versionStatus: string;
    entityId: number;
    entityNumber: number;
    travelEntityName: string;
    entityEmail: string;
    entityAddress: string;
    entityIsActive: boolean;
    travelTitle: string;
    purposeOfTheTrip: string;
    travelAmount: number;
    travelStartDate: number;
    travelEndDate: number;
    destinationCity: string;
    destinationCountry: string;
    travelState: string;
    relationshipToYourResearch: string;
    acknowledgeBy: string;
    acknowledgeAt: number;
    updateTimestamp: number;
    updateUser: string;
    createUser: string;
    createTimestamp: number;
    travellerHomeUnit: string;
    description: string;
    travelSubmissionDate: number;
    dispositionStatus: string;
    dispositionStatusCode: string;
    reviewStatus: string;
    reviewStatusCode: string;
    adminPersonId: string;
    adminGroupId: number;
    adminPersonName: string;
    adminGroupName: string;
    homeUnitNumber: string;
    homeUnitName: string;
    isInterNationalTravel: boolean;
    travellerTypeCodeList: {};
    personId: string;
    personFullName: string;
    entityTypeCode: string;
    entityType: string;
    countryCode: string;
    country: string;
    certifiedBy: string;
    certifiedAt: number;
    documentStatusCode: string;
    documentStatus: string;
    disclosureStatusCode: string;
    disclosureStatus: string;
    riskLevel: string;
    expirationDate: number;
    riskCategoryCode: string;
    comment: any;
    entityRiskCategory: EntityRiskCategory;
    personPrimaryTitle: string;
    personEmail: string;
    travelNumber: number;
    personEntitiesCount: number = 0;
    personNotesCount: number = 0;
    personAttachmentsCount: number = 0;
}

export interface EndpointOptions {
    contextField: string;
    formatString: string;
    path: string;
    defaultValue: string;
    params: string;
}

export class EntityDetails {
    isActive: boolean;
    country: Country;
    entityId: number;
    entityType: EntityType | any;
    entityName: string;
    emailAddress: string;
    address: string;
    entityRiskCategory?: EntityRiskCategory;
    entityNumber?: any;
}

export interface TravelHistoryRO {
    personId: string;
    entityNumber: number;
}

export interface TravelHistory {
    travelDisclosureId: number;
    travelEntityName: string;
    entityType: string;
    country: string;
    travelTitle: string;
    purposeOfTheTrip: string;
    destinationCity: string;
    destinationCountry: string;
    destinationState: string;
    travellerTypeCodeList: [];
    travelAmount: number;
    travelStartDate: number;
    travelEndDate: number;
}

export interface TravelActionAfterSubmitRO {
    travelDisclosureId: number;
    description: string;
}

export class TravelConflictRO {
    travelDisclosureId: number;
    personId: string;
    description: string;
    disclosureStatusCode: string;
}

export interface EntityRiskCategory {
    description: string;
    riskCategoryCode: string;
    isActive?: boolean;
    updateUser?: string;
    updateTimestamp?: number;
    sortOrder?: string;
}

export interface CreateTravelDisclosure {
    travelDisclosureId: number
    travelNumber: number
    versionNumber: number
    versionStatus: string
    personEntityId: number
    personEntity: PersonEntity
    entityId: number
    entityNumber: number
    personId: string
    person: Person
    travelStatusCode: string
    coiTravelerStatusType: CoiTravelerStatusType
    riskCategoryCode: string
    coiRiskCategory: EntityRiskCategory
    isSponsoredTravel: boolean
    isInterNationalTravel: boolean
    travelTitle: string
    purposeOfTheTrip: string
    travelAmount: number
    travelStartDate: number
    travelEndDate: number
    noOfDays: number
    destinationCity: string
    destinationCountry: string
    travelstate: string
    relationshipToYourResearch: string
    acknowledgeBy: any
    acknowledgeAt: any
    updateTimestamp: number
    updateUser: string
    createUser: string
    createTimestamp: number
    coiTravellerTypeCodeList: string[]
    travellerHomeUnit: string
    description: any
    travelSubmissionDate: any
    disclosureStatusCode: string
    coiTravelDisclosureStatusType: CoiTravelDisclosureStatusType
    dispositionStatusCode: any
    reviewStatusCode: string
    travellerTypeCode: any
    adminGroupId: any
    adminPersonId: any
    certifiedBy: any
    certifiedAt: any
    documentStatusCode: string
    expirationDate: any
    travellerUnitDetails: Unit
    coiTravelDisclosureStatusTypeDetalis: CoiTravelDisclosureStatusType
    adminGroupName: any
    adminPersonName: any
    personFullName: string
    travellerTypeCodeList: {}
    coiDocumentStatusTypeDetalis: CoiDocumentStatusTypeDetalis
    coiTravelReviewStatusTypeDetails: CoiTravelReviewStatusTypeDetails
    coiTravelDisclosureStatusList: any
    entityDetails: EntityDetails
    riskLevel: string
    personNotesCount: number
    personAttachmentsCount: number
    personEntitiesCount: number
    coiEntity: CoiEntity
}

export interface CoiTravelerStatusType {
    travelStatusCode: string
    description: string
    updateTimestamp: number
    updateUser: string
    isActive: boolean
}

export interface CoiTravelDisclosureStatusType {
    disclosureStatusCode: string
    description: string
    updateTimestamp: number
    updateUser: string
    isActive: boolean
    sortOrder: string
}

export interface CoiDocumentStatusTypeDetalis {
    documentStatusCode: string
    description: string
    updateTimestamp: number
    updateUser: string
    isActive: boolean
}

export interface CoiTravelReviewStatusTypeDetails {
    reviewStatusCode: string
    description: string
    updateTimestamp: number
    updateUser: string
    isActive: boolean
}

export interface PersonEntity {
    personEntityId: number
    personEntityNumber: number
    personId: string
    person: Person
    entityId: number
    coiEntity: CoiEntity
    entityNumber: number
    isFormCompleted: boolean
    versionNumber: number
    versionStatus: string
    sponsorsResearch: any
    involvementStartDate: number
    involvementEndDate: any
    studentInvolvement: any
    staffInvolvement: any
    instituteResourceInvolvement: any
    updateTimestamp: number
    updateUser: string
    createUser: string
    createTimestamp: number
    revisionReason: any
    personEntityRelationships: any
    validPersonEntityRelTypes: any
    validPersonEntityRelTypeCodes: any
    personFullName: any
    unit: any
    relationshipTypes: any
    designation: any
    updateUserFullName: any
    personEntityRelationshipDto: any
    disclosureId: any
    canDelete: any
    sfiCompleted: any
    disclosureStatusCount: any
}

export interface Person {
    personId: string
    lastName: string
    firstName: string
    middleName: any
    fullName: string
    priorName: any
    principalName: string
    emailAddress: string
    dateOfBirth: any
    age: any
    gender: any
    educationLevel: any
    officeLocation: any
    secOfficeLocation: any
    secOfficePhone: any
    school: any
    directoryDepartment: any
    countryOfCitizenshipCode: any
    countryOfCitizenshipDetails: any
    primaryTitle: string
    directoryTitle: string
    homeUnit: string
    unit: Unit
    isFaculty: boolean
    isGraduateStudentStaff: boolean
    isResearchStaff: boolean
    isServiceStaff: boolean
    isSupportStaff: boolean
    isOtherAcadamic: boolean
    isMedicalStaff: boolean
    addressLine1: any
    addressLine2: any
    addressLine3: any
    city: string
    country: string
    state: string
    postalCode: string
    countryCode: string
    countryDetails: Country
    faxNumber: any
    pagerNumber: any
    mobileNumber: string
    status: string
    salaryAnniversary: any
    updateTimestamp: number
    updateUser: string
    supervisorPersonId: any
    orcidId: string
    isWebhookActive: any
    dateOfInactive: any
    isExternalUser: any
    officePhone: any
    isPasswordChange: boolean
    isUsernameChange: boolean
}

export interface Unit {
    unitNumber: string
    parentUnitNumber: any
    organizationId: string
    unitName: string
    active: boolean
    updateTimestamp: number
    updateUser: string
    acronym: any
    isFundingUnit: any
    unitAdministrators: UnitAdministrator[]
    unitDetail: string
    parentUnitName: any
    organizationName: any
}

export interface UnitAdministrator {
    personId: string
    fullName: any
    oldPersonId: any
    oldUnitAdministratorTypeCode: any
    unitAdministratorTypeCode: string
    unitNumber: string
    unitName: any
    updateTimestamp: number
    updateUser: string
    unitAdministratorType: UnitAdministratorType
}

export interface UnitAdministratorType {
    code: string
    description: string
    isActive: boolean
}

export interface Currency {
    currencyCode: string
    currency: string
    currencySymbol: string
    updateUser: string
    updateTimeStamp: number
}

export interface CoiEntity {
    entityId: number
    entityNumber: number
    entityName: string
    versionNumber: number
    versionStatus: string
    entityStatusCode: string
    entityStatus: EntityStatus
    entityTypeCode: string
    entityType: EntityType
    riskCategoryCode: string
    entityRiskCategory: EntityRiskCategory
    phone: any
    countryCode: string
    country: Country
    city: any
    address: string
    zipCode: any
    emailAddress: any
    isActive: boolean
    webURL: any
    createUser: string
    createTimestamp: number
    updateUser: string
    updateTimestamp: number
    approvedUser: any
    approvedTimestamp: any
    revisionReason: any
    countryDescription: any
    entityTypeDescription: any
    riskLevelDescription: any
    statusDescription: any
    updatedUserFullName: any
    createUserFullName: any
    majorVersion: boolean
    newRiskCategory: any
}

export interface EntityStatus {
    entityStatusCode: string
    description: string
    updateTimestamp: number
    updateUser: string
    isActive: boolean
}

export interface EntityType {
    entityTypeCode: string
    description: string
    updateTimestamp: number
    updateUser: string
    isActive: boolean
}

export interface Country {
    countryCode?: string
    countryName?: string
    currencyCode?: string
    currency?: Currency
    updateTimeStamp?: number
    updateUser?: string
    countryTwoCode?: string
}
