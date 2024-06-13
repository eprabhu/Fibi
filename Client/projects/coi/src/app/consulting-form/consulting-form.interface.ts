export class ConsultingForm {
    consultingFormDisclosure: ConsultingFormDisclosure;
}

export class ConsultingFormDisclosure{
    disclosureId: number;
    personId: number;
    person: Person;
    personEntityId: number;
    personEntity: any;
    entityNumber: any;
    homeUnit: any;
    unit: Unit;
    reviewStatusCode: string;
    reviewStatusType: ReviewStatusType;
    dispositionStatusCode: string;
    dispositionStatusType: DispositionStatusType;
    certificationText: any;
    certifiedBy: string;
    certifiedAt: number;
    expirationDate: number;
    adminGroupId: any;
    adminPersonId: any;
    createTimestamp: number;
    createUser: string;
    updateTimeStamp: number;
    updateUser: string;
    updateUserFullName: string;
    createUserFullName: string;
    adminGroupName: null;
    adminPersonName: null;
    homeUnitName: any;
    consultingDisclFormBuilderDetails: any[];
    personNotesCount: number;
    personAttachmentsCount: number;
    personEntitiesCount: number;
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

export interface UnitAdministratorType {
    code: string;
    description: string;
    isActive: boolean;
}


export interface Person {
    personId: string;
    personName: string;
    formOfAddressShort?: any;
    firstName: string;
    middleName: string;
    lastName: string;
    fullName: string;
    krbNameUppercase?: any;
    emailAddress?: any;
    jobId?: any;
    jobTitle?: any;
    adminEmployeeType?: any;
    hrDepartmentCodeOld?: any;
    hrDepartmentName?: any;
    adminOrgUnitId?: any;
    adminOrgUnitTitle?: any;
    adminPositionTitle?: any;
    payrollRank?: any;
    isFaculty: boolean;
    employmentPercent?: any;
    isConsultPriv?: any;
    isPaidAppt?: any;
    isSummerSessionAppt?: any;
    summerSessionMonths?: any;
    isSabbatical?: any;
    sabbaticalBeginDate?: any;
    sabbaticalEndDate?: any;
    warehouseLoadDate?: any;
    directoryTitle: any;
}

export interface ReviewStatusType {
    reviewStatusCode: string;
    description: string;
    updateTimestamp: number;
    updateUser: string;
    isActive: boolean;
}

export interface DispositionStatusType {
    dispositionStatusCode: string;
    description: string;
    updateTimestamp: number;
    updateUser: string;
    isActive: boolean;
}
