export class AssignAdminRO {
    adminPersonId;
    adminGroupId = null;
    travelDisclosureId?: '';
    opaDisclosureId?: '';
    opaDisclosureNumber?: '';
    disclosureId?: '';
}

export class DefaultAssignAdminDetails {
    adminPersonId = '';
    adminGroupId = null;
    adminPersonName = '';
    adminGroupName = '';
}

export class PersonProjectOrEntity {
    personFullName = '';
    projectDetails ?: any = {};
    entityName ? = '';
    unitDetails = '';
}

export class Disclosure {
    adminGroupId: any;
    adminPersonId: any;
    certifiedAt: any;
    conflictStatus: any;
    conflictStatusCode: any;
    createTimestamp: any;
    createUserFullName: any;
    disclosureId: any;
    disclosureNumber: any;
    dispositionStatus: any;
    dispositionStatusCode: any;
    expirationDate: any;
    homeUnit: any;
    homeUnitName: any;
    personId: any;
    reviewStatus: any;
    reviewStatusCode: any;
    updateTimestamp: any;
    updateUserFullName: any;
    versionNumber: any;
    versionStatus: any;
    type: any;
    disclosurePersonFullName: any;
    disclosureType: any;
}

export class RevisionObject {
    revisionComment: null;
    disclosureId: null;
    homeUnit: null;
}

export class SfiObject {
    isActive = 'INACTIVE';
    validPersonEntityRelTypes = [];
    entityType = '';
    involvementStartDate = '';
    involvementEndDate = '';
    countryName = '';
    entityId = '';
    entityName = '';
  }

export interface coiReviewComment {
    documentOwnerPersonId:string;
    componentTypeCode : any;
    subModuleItemKey?: any;
    subModuleItemNumber?:any;
    sfiStatus?:any;
    selectedProject?:any;
    coiSubSectionsTitle?:any;
    headerName?:any;
    subSectionTitle?:any;
    subSectionId?:any;
}
