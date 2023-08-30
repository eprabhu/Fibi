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
}
             