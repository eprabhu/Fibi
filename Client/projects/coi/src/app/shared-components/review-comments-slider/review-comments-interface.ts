export class ReviewComments {
    coiReviewCommentDto: CoiReviewComment = new CoiReviewComment();
    documentOwnerPersonId: number;
}


export class CoiReviewComment {
    coiReviewCommentId: any;
    coiSectionsTypeCode: string;
    disclosureId: number;
    comment: string = '';
    coiParentCommentId: number = null;
    isPrivate: boolean = false;
    coiSubSectionsId: number = null;
    coiReviewCommentTag: any = [];
    coiReviewCommentAttachment: any = [];
    componentSubRefId: number = null;
    commentId: number = null;
    // isAdmin = false;
    // isReviewer = false;
    // isReporter = false;
}

export class RO {
    'disclosureId' = null;
    'reviewStatusCode' = '';
    'personId': any;
    'filterType': string =  'ALL';
    'currentPage': any = 1;
    'pageNumber': any = 20;
    'searchWord': any;
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
export class CompleterOptions {
    arrayList: any[];
    contextField: string;
    filterFields: string;
    formatString: string;
    defaultValue = '';
}
