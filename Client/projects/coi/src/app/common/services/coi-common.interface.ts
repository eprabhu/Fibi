import { AttachmentInputType, COIAttachment } from "../../attachments/attachment-interface";

export type Method = 'SOME' | 'EVERY';
export type GlobalEventNotifierUniqueId = 'CREATE_NEW_TRAVEL_DISCLOSURE' | 'COI_OPA_HEADER' | 'COI_DISCLOSURE_HEADER_RESIZE' | 'SCROLL_SPY' | 'COI_DISCLOSURE_ADD_CONFLICT_UPDATE';
export type GlobalEventNotifier = { uniqueId: GlobalEventNotifierUniqueId, content?: any };
export type LoginPersonDetailsKey = keyof LoginPersonDetails;

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
