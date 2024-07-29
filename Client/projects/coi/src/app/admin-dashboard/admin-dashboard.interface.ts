
export class ProjectOverview {
    projectOverviewDetails: ProjectOverviewDetails[] = [];
    proposalCount: number | null = null;
}

export interface ProjectOverviewDetails {
    keyPersonCount: number;
    keyPersonDetails: KeyPersonDetail[];
    projectDetails: ProjectDetails;
}
export interface KeyPersonDetail {
    moduleCode: any
    projectId: any
    projectNumber: any
    title: any
    projectStatus: any
    projectStartDate: any
    projectEndDate: any
    homeUnitNumber: string
    homeUnitName: string
    leadUnitNumber: any
    leadUnitName: any
    sponsorName: any
    sponsorCode: any
    primeSponsorName: any
    primeSponsorCode: any
    piName: any
    keyPersonId: string
    keyPersonName: string
    keyPersonRole: string
    reporterRole: any
    conflictStatus: any
    conflictStatusCode: any
    entityCount: any
    relationShipExists: any
    sfiCompleted: any
    disclosureStatusCount: any
    projectTypeCode: any
    projectType: any
    projectBadgeColour: any
    disclosureSubmitted: boolean
    questionnaireCompleted: any
    disclsoureNeeded: any
    disclosureReviewStatus?: string
    proposalStatus: any
    inCompletCount: any
    completeCount: any
    disclosureId: number
}

export interface ProjectDetails {
    moduleCode: any
    projectId: string
    projectNumber: any
    title: string
    projectStatus: string
    projectStartDate: number
    projectEndDate: number
    homeUnitNumber: any
    homeUnitName: any
    leadUnitNumber: string
    leadUnitName: string
    sponsorName: string
    sponsorCode: string
    primeSponsorName?: string
    primeSponsorCode?: string
    piName: any
    keyPersonId: any
    keyPersonName: any
    keyPersonRole: any
    reporterRole: any
    conflictStatus: any
    conflictStatusCode: any
    entityCount: any
    relationShipExists: any
    sfiCompleted: any
    disclosureStatusCount: any
    projectTypeCode: string
    projectType: string
    projectBadgeColour: any
    disclosureSubmitted: any
    questionnaireCompleted: boolean
    disclsoureNeeded: any
    disclosureReviewStatus: any
    proposalStatus: string
    inCompletCount: number
    completeCount: number
    disclosureId: any
    updateTimestamp: any
    proposalCount: any
    commentCount: number;
}

export class CoiProjectOverviewRequest {
    property2 = null;
    property3 = null;
    property6 = null;
    property4 = [];
    property5 = [];
    property9 = null;
    property11 = null;
    property13 = null;
    property14 = null;
    personId = null;
    pageNumber = 20;
    currentPage = 1;
    isDownload = true;
    advancedSearch = 'L';
  }
  
  export class CoiProjectOverviewComment {
    commentTypeCode: any = null;
    commentType: any = null;
    parentCommentId: any = null;
    isPrivate:boolean = false;
    comment: any = null;
    moduleItemKey: any = null;
    moduleCode: any = null;
    commentId: any = null;
  }
  
  export class projectOverviewCommentFetch {
    commentTypeCode: any = null;
    moduleCode: any = null;
    moduleItemKey: any = null;
    parentCommentId: any = null;
  }
