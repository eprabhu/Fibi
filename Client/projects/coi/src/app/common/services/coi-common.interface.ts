import { AttachmentInputType, COIAttachment } from "../../attachments/attachment-interface";

export type Method = 'SOME' | 'EVERY';
export type FcoiType = 'INITIAL' | 'REVISION' | 'PROJECT';
export type GlobalEventNotifierUniqueId = 'CREATE_NEW_TRAVEL_DISCLOSURE' | 'COI_OPA_HEADER' | 'COI_DISCLOSURE_HEADER_RESIZE' | 'SCROLL_SPY' | 'COI_DISCLOSURE_ADD_CONFLICT_UPDATE';
export type GlobalEventNotifier = { uniqueId: GlobalEventNotifierUniqueId, content?: any };
export type LoginPersonDetailsKey = keyof LoginPersonDetails;

export class COIAppConfig {
    baseUrl = '';
    fibiUrl = '';
    authUrl = '';
    opaUrl = '';
    formUrl = '';
    fibiCOIConnectUrl = '';
    entityURL = '';
    enableSSO = false;
    enableGraph = true;
    isElasticAuthentiaction = false;
    elasticUserName = '';
    elasticDelimiter = '';
    elasticPassword = '';
    elasticAuthScheme = '';
    elasticIndexUrl = '';
    indexValue = '';
    fibiApplicationUrl = '';
    EXTERNAL_APPLICATION_BASE_URL = '';
    EXTERNAL_DEV_PROPOSAL_URL = '';
    EXTERNAL_AWARD_URL = '';
    EXTERNAL_IP_URL = '';
    EXTERNAL_PERSON_URL: '';
    EXTERNAL_ROLODEX_PERSON_URL: '';

    constructor(init?: Partial<COIAppConfig>) {
        Object.assign(this, init);
    }
}

export class LoginPersonDetails {
    personID: any;
    userName: any;
    firstName: any;
    lastName: any;
    fullName: any;
    unitNumber: any;
    unitName: any;
    primaryTitle: any;
    email: any;
    isUnitAdmin: any;
    login: any;
    userType: any;
    secretImageUri: any;
    isExternalUser: any;
    gender: any;
}

export interface DashboardProjectCount {
    moduleCode: number,
    projectType: string,
    projectCount: number
}

export class CoiAttachmentModalInfo {
    isOpenAttachmentModal = false;
    attachmentModalInputType: AttachmentInputType;
    coiCurrentAttachment: COIAttachment;
}

export interface AttachmentSaveRO {
    fileName: string;
    mimeType: string;
    attachmentTypeCode?: string | number;
    description?: string;
    fileDataId: string | null;
    comment?: string;
    attaTypeCode?: string | number;
}

export interface AttachmentReplaceRO {
    fileName: string;
    mimeType: string;
    attachmentTypeCode?: string | number;
    description?: string;
    fileDataId: string | null;
    attachmentNumber: number;
    versionNumber: number;
    comment?: string;
    attaTypeCode?: string | number;
}

export class CoiDisclosureCount {
    inProgressDisclosureCount?: number = 0;
    approvedDisclosureCount?: number = 0;
    travelDisclosureCount?: number = 0;
    consultDisclCount?: number = 0;
    disclosureHistoryCount?: number = 0;
    commentCount?: number = 0;
}

export class SharedProjectDetails {
    projectNumber?: string = null;
    sponsorCode?: string = null;
    primeSponsorCode?: string = null;
    sponsorName?: string = null;
    homeUnitName?: string = null;
    homeUnitNumber?: string = null;
    primeSponsorName?: string = null;
    projectStatus?: string = null;
    piName?: string = null;
    projectStartDate?: number = null;
    projectEndDate?: number = null;
    projectBadgeColour?: string = null;
    projectIcon?: string = null;
    projectType?: string = null;
    projectTypeCode?: string = null;
    projectTitle?: string = null;
    documentNumber?: string = null;
    accountNumber?: string = null;
    projectId?: string = null;
}
