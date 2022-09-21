export class ExtReviewer {
    extReviewerId?: string | number;
    firstName? = '';
    lastName? = '';
    middleName? = '';
    passportName? = '';
    gender?= null;
    primaryEmail? = '';
    secondaryEmail?: string;
    principalName?: string;
    academicRankCode?: string | number = null;
    academicRank?: any;
    workCountry?: string;
    agreementStartDate?: number | Date | string;
    agreementEndDate?: number | Date | string;
    scoringTrend?: string;
    countryCode?: number;
    countryDetails?: any;
    acType?: any;
    status?: any = 'A';
    isUsernameChange?: boolean;
    academicAreaCodePrimary?: string | number = null;
    academicAreaPrimary?: any;
    academicAreaCodeSecondary?: string | number = null;
    academicAreaSecondary?: any;
    affilationInstitutionCode?: string | number = null;
    affilationInstitution?: any;
    isTopInstitution?= null;
    externalReviewerId?: number;
    department?: string;
}

export class ExternalReviewerExt {
    externalReviewerId?: string | number;
    hIndex?: string | number = null;
    coiWithPerson?: string;
    supplierDof?: string;
    ciraCode?: string | number = null;
    extReviewerCira?: any;
    extReviewerOriginality?: any;
    orginalityCode?: string | number = null;
    extReviewerThoroughness?: any;
    thoroughnessCode?: string | number = null;
    scopusUrl? = '';
    urlProfile? = '';
    disciplinaryField?: string;
}

export class AttachmentType {
    attachmentTypeCode: number;
    description: string;
    updateTimeStamp: number;
    updateUser: string;
    isActive: boolean;
}

export class ExtReviewerAttachment {
    externalReviewerId: number | string;
    attachmentTypeCode: number | string;
    externalReviewerAttachmentType = new AttachmentType();
    description: string;
    mimeType: string;
    fileName: string;
}
export class ExternalReviewerRight {
    externalReviewerId?: string | number = '';
    personRoleId?: string = null;
    reviewerRightId?: string = null;
    reviewerRights?: any;
}


