import { DashboardProjectCount } from "../common/services/coi-common.interface";

export class AssignAdminRO {
    adminPersonId;
    adminGroupId = null;
    travelDisclosureId?: '';
    opaDisclosureId?: '';
    opaDisclosureNumber?: '';
    disclosureId?: '';
    actionType?: 'R' | 'A';
}

export class DefaultAssignAdminDetails {
    adminPersonId = '';
    adminGroupId = null;
    adminPersonName = '';
    adminGroupName = '';
}

export class PersonProjectOrEntity {
    personFullName = '';
    projectDetails?: any = null;
    entityName? = '';
    homeUnit? = '';
    homeUnitName? = '';
    personEmail = '';
    personPrimaryTitle = '';
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
    isActive = false;
    validPersonEntityRelTypes = [];
    entityType = '';
    involvementStartDate = '';
    involvementEndDate = '';
    countryName = '';
    entityId = '';
    entityNumber = '';
    entityName = '';
    canDelete: boolean;
    isFormCompleted: false;
}

export interface coiReviewComment {
    documentOwnerPersonId: string;
    componentTypeCode: any;
    subModuleItemKey?: any;
    subModuleItemNumber?: any;
    sfiStatus?: any;
    selectedProject?: any;
    coiSubSectionsTitle?: any;
    headerName?: any;
    subSectionTitle?: any;
    subSectionId?: any;
}

export class DisclosureProjectData {
    title?: string = '';
    piName?: string = '';
    projectId?: string = '';
    projectType?: string = '';
    sponsorName?: string = '';
    reporterRole?: string = '';
    homeUnitName?: string = '';
    projectStatus?: string = '';
    projectNumber?: string = '';
    homeUnitNumber?: string = '';
    projectTypeCode?: string = '';
    primeSponsorName?: string = '';
    projectBadgeColour?: string = '';
    projectEndDate?: number | null = null;
    projectStartDate?: number | null = null;
    sponsorCode?: string = '';
    accountNumber?: string = '';
    documentNumber?: string = '';
    primeSponsorCode?: string = '';
    entityCount?: any;
    leadUnitName?: any;
    disclosureId?: any;
    completeCount?: any;
    keyPersonRole?: any;
    moduleCode?: number;
    keyPersonId?: string;
    leadUnitNumber?: any;
    proposalStatus?: any;
    inCompletCount?: any;
    disclsoureNeeded?: any;
    sfiCompleted?: boolean;
    keyPersonName?: string;
    conflictStatus?: string;
    projectIcon?: string = '';
    disclosureSubmitted?: any;
    conflictStatusCode?: string;
    relationShipExists?: boolean;
    questionnaireCompleted?: any;
    disclosureReviewStatus?: any;
    projectConflictStatus?: string;
    projectConflictStatusCode?: string;
    disclosureStatusCount?: any[] = [];
}

export interface CountModalDisclosureProjectData extends DisclosureProjectData {
    formattedLeadUnit: string;
    formattedSponsor: string;
    formattedPrimeSponsor: string;
    formattedProjectHeader: string;
    disclosureConflictBadge: string;
}

export class DisclosureProjectModalData {
    projectDetails: DisclosureProjectData | null = null;
    coiDisclosureId: number | null = null;
    needReporterRole: boolean = true;
}

export interface AssignAdminGroup {
    adminGroupId: number
    adminGroupName: string
    description: string
    email: any
    roleId: number
    role: Role
    primaryPersonId: any
    person: any
    isActive: boolean
    updateTimestamp: number
    updateUser: string
    moduleCode: number
}

export interface Role {
    roleId: number
    roleName: string
    description: string
    statusFlag: string
    roleTypeCode: string
    roleType: RoleType
    createTimeStamp: number
    createUser: string
    updateTimeStamp: number
    updateUser: string
}

export interface RoleType {
    roleTypeCode: string
    roleType: string
    isActive: boolean
}

export interface FCOIDisclosureCreateRO {
    coiProjectTypeCode?: string
    revisionComment?: string
    moduleItemKey?: number
    fcoiTypeCode?: string
    moduleCode?: number
    homeUnit: string
    personId: string
}

export interface COICountModalViewSlider {
    isOpenSlider: boolean;
    entityId: number | string;
}

export interface COICountModalClose {
    isOpenCountModal: boolean;
    content?: number | string;
}

export class COICountModal {
    inputType: 'SFI_TAB' | 'DISCLOSURE_TAB' | '' = '';
    moduleCode: number | null = null;
    fcoiTypeCode: string | null = null;
    projectHeader?: string | null = null;
    projectTitle?: string | null = null;
    projectNumber?: string | null = null;
    personFullName: string | null = null;
    disclosureType: string | null = null;
    personUnit: {} | null = null;
    disclosureId: number | null = null;
    personId: string | null = null;
    isOpenCountModal = false;
}

export interface COICountModalProjectUpdate {
    projectCountList: DashboardProjectCount[];
    updatedProjectsList: CountModalDisclosureProjectData[];
}

export interface CoiProjectType {
    coiProjectTypeCode?: string;
    description?: string;
    badgeColor?: string;
    projectIcon?: string;
    fcoiNeeded?: boolean;
    projectDisclosureNeeded?: boolean;
    updateTimestamp?: number;
    updateUser?: string;
}
