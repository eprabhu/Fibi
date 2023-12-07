export class OPA {
    opaDisclosure: OpaDisclosure;
    opaReviewerList: any;
}

export class OpaDisclosure {
    opaDisclosureId: number;
    opaDisclosureNumber: string;
    opaCycleNumber: number;
    opaCycles: OpaCycles;
    personId: string;
    opaPerson: OpaPerson;
    personName: string;
    homeUnit: string;
    statusCode: string;
    isFaculty: boolean;
    isFallSabatical?: any;
    isSpringSabatical?: any;
    receivedSummerComp?: any;
    summerCompMonths?: any;
    isFullTime?: any;
    isPartTime?: any;
    appointmentPercent?: any;
    isCompensated?: any;
    hasPotentialConflict?: any;
    conflictDescription?: any;
    reviewStatusCode: string;
    reviewStatusType: reviewStatusType;
    dispositionStatusCode: string;
    dispositionStatusType: DispositionStatusType;
    certificationText: string;
    certifiedBy: string;
    submissionTimestamp: number;
    adminGroupId: number;
    adminPersonId: string;
    createTimestamp: number;
    createUser: string;
    updateTimestamp: number;
    updateUser: string;
    updateUserFullName: string;
    createUserFullName?: any;
    adminGroupName: string;
    adminPersonName: string;
    personEmail: string;
    personPrimaryTitle: string;
    homeUnitName: string;
    opaFormBuilderDetails: any[];
    personAttachmentsCount: number;
    personNotesCount: number;
    personEntitiesCount: number;
}

export interface DispositionStatusType {
    dispositionStatusCode: string;
    description: string;
    updateTimestamp: number;
    updateUser: string;
    isActive: boolean;
}

export interface reviewStatusType {
    reviewStatusCode: string;
    description: string;
    updateTimestamp: number;
    updateUser: string;
    isActive: boolean;
}

export interface OpaPerson {
    personId: string;
    personName: string;
    formOfAddressShort?: any;
    firstName: string;
    middleName: string;
    lastName: string;
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
}

export interface OpaCycles {
    opaCycleNumber: number;
    periodStartDate: number;
    periodEndDate: number;
    opaCycleStatus: boolean;
    openDate: number;
    closeDate: number;
    updateTimestamp: number;
    updateUser: string;
}
