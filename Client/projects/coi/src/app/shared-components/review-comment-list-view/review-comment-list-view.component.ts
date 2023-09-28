import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { ReviewComments } from '../review-comments-slider/review-comments-interface';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { ReviewCommentsService } from '../review-comments-slider/review-comments.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { fileDownloader } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';

@Component({
	selector: 'app-review-comment-list-view',
	templateUrl: './review-comment-list-view.component.html',
	styleUrls: ['./review-comment-list-view.component.scss']
})
export class ReviewCommentListViewComponent implements OnInit, OnDestroy, OnChanges {

	@Input() commentReviewerList: any = [];
	@Input() reviewTypeList: any = [];
	@Input() selectedReviewType:any;
	@Output() deleteReviewComment: EventEmitter<any> = new EventEmitter<any>();
	@Output() editReviewParentComment: EventEmitter<any> = new EventEmitter<any>();
	@Output() emitReplayCommentDetails: EventEmitter<any> = new EventEmitter<any>();
	@Output() deleteChidReviewComment: EventEmitter<any> = new EventEmitter<any>();
	isEditComment = false;
	commentDetails: ReviewComments = new ReviewComments();
	$subscriptions: Subscription[] = [];
	isReplyComment = false;
	mandatoryMap = new Map();
	selectedCommentIdList:any = [];
	isShowReplyComment = false;
	readMoreCommentIdList:any = [];
	isReadMore = false;




	constructor(private _commonService: CommonService, public reviewCommentsService: ReviewCommentsService) { }

	ngOnInit() {

	}

	ngOnChanges() {

	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}

	deleteComment(details, index) {
		this.$subscriptions.push(this.reviewCommentsService.deleteReviewComments(details.commentId).subscribe((res: any) => {
			details.parentCommentId ? this.deleteReplyComment(details,index) : this.deleteReviewComment.emit(index);
			this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Comment deleted successfully')
		}, error => {
			this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')
		}));
	}

	editParentComment(details, index) {
		if (!this.reviewCommentsService.isEditParentComment && !this.isEditComment && !this.isReplyComment) {
			this.reviewCommentsService.isEditParentComment = true;
			this.reviewCommentsService.editParentCommentId = details.commentId;
			this.editReviewParentComment.emit(details);
		}
	}

	replyComment(commentDetails, index) {
		if(!this.isReplyComment && !this.reviewCommentsService.isEditParentComment && !this.isEditComment) {
			this.getReviewerActionDetails();
			this.commentDetails.coiReviewCommentDto.coiParentCommentId = commentDetails.commentId;
			this.commentDetails.coiReviewCommentDto.coiSubSectionsId = commentDetails.componentReferenceNumber;
			this.commentDetails.coiReviewCommentDto.componentSubRefId = commentDetails.componentSubReferenceId;
			this.isReplyComment = true;
		}

	}

	getReviewerActionDetails() {
		this.$subscriptions.push(this._commonService.$commentConfigurationDetails.subscribe((res: any) => {
			this.commentDetails.documentOwnerPersonId = res.documentOwnerPersonId;
			this.commentDetails.coiReviewCommentDto.disclosureId = res.disclosureId;
			this.commentDetails.coiReviewCommentDto.coiSectionsTypeCode = res.coiSectionsTypeCode;
		}));
	}

	editReplyComment(replayComment) {
		if( !this.isEditComment  &&  !this.reviewCommentsService.isEditParentComment && !this.isReplyComment) {
			this.isEditComment = true;
			this.commentDetails.coiReviewCommentDto.disclosureId = replayComment.componentReferenceId
			this.commentDetails.coiReviewCommentDto.commentId = replayComment.commentId;
			this.commentDetails.coiReviewCommentDto.comment = replayComment.comment;
			this.commentDetails.coiReviewCommentDto.coiParentCommentId = replayComment.parentCommentId;
			this.commentDetails.coiReviewCommentDto.coiSubSectionsId = replayComment.componentReferenceNumber;
			this.commentDetails.coiReviewCommentDto.componentSubRefId = replayComment.componentSubReferenceId;
		}
	}

	deleteReplyComment(replayComment, index) {
		this.deleteChidReviewComment.emit({commentId:replayComment.commentId, index: index})
	}

	cancelOrClearCommentsDetails() {
		this.commentDetails = new ReviewComments();
		if (this.isEditComment) {
			this.isEditComment = false;
		}
		if(this.isReplyComment) {
			this.isReplyComment = false;
		}
		this.getReviewerActionDetails();
	}

	addReplayCommentsDetails() {
		if(!this.validateComment()) {
            this.emitReplayCommentDetails.emit({details:this.commentDetails});
            this.cancelOrClearCommentsDetails();
            }
	}

	validateComment() {
        this.mandatoryMap.clear();
        if (this.commentDetails.coiReviewCommentDto.comment === '' ) {
            this.mandatoryMap.set('comment', 'Please add comment');
        }
        return this.mandatoryMap.size === 0 ? false : true;
    }
	
	downloadAttachment(attachment) {
        this.$subscriptions.push(this.reviewCommentsService.downloadAttachment(attachment.attachmentId).subscribe(res => {
            fileDownloader(res,attachment.fileName);
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Attachment downloaded successfully');
        },error =>{
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')
        }));
    }

	showReplyComment(details, actionType) {
		if (details?.reply?.length) {
			if (actionType == 'SHOW') {
				this.selectedCommentIdList.push(details.commentId);
				this.isShowReplyComment = true;
			} else {
				const INDEX = this.selectedCommentIdList.findIndex(ele => ele === details.commentId);
				this.selectedCommentIdList.splice(INDEX,1);
				this.isShowReplyComment = this.selectedCommentIdList.length ?  true : false; 
			}
		}
	}
	readMore(details,actionType) {
			if (actionType == 'MORE') {
				this.readMoreCommentIdList.push(details.commentId);
			} else {
				const INDEX = this.readMoreCommentIdList.findIndex(ele => ele === details.commentId);
				this.readMoreCommentIdList.splice(INDEX,1);
			}
	}

}
