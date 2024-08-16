import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { getFormattedSponsor, openCoiSlider } from '../../common/utilities/custom-utilities';
import { EDITOR_CONFIURATION, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from 'projects/fibi/src/app/app-constants';
import * as DecoupledEditor from '@ckeditor/ckeditor5-build-decoupled-document';
import { ProjectOverviewService } from '../project-overview.service';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { CoiProjectOverviewComment, projectOverviewCommentFetch } from '../admin-dashboard.interface';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

@Component({
    selector: 'app-project-overview-comments-slider',
    templateUrl: './project-overview-comments-slider.component.html',
    styleUrls: ['./project-overview-comments-slider.component.scss']
})
export class ProjectOverviewCommentsSliderComponent implements OnInit {

    @Input() dataForCommentSlider: any;
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();
    @Output() commentCountUpdated = new EventEmitter<number>();
    public Editor = DecoupledEditor;
    editorConfig = EDITOR_CONFIURATION;
    isReplyComment = false;
    projectOverviewCommentDetails: CoiProjectOverviewComment = new CoiProjectOverviewComment();
    $subscriptions: Subscription[] = [];
    showAddComment = false;
    isEditComment = false;
    visibleCommentsMap: { [key: number]: any[] } = {};
    initialVisibleComments = 2;
    public currentEditIndex: number | null = null;
    commentFetchRequestPayload: projectOverviewCommentFetch = new projectOverviewCommentFetch();
    commentsData: any[] = [];
    mandatoryMap = new Map();
    currentUserId: any;
    getFormattedSponsor = getFormattedSponsor;

    constructor(public projectOverviewService: ProjectOverviewService, public commonService: CommonService) { }

    ngOnInit() {
        setTimeout(() => {
            openCoiSlider('coi-project-overview-slider');
        });
        this.loadProjectOverviewCommentBody();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }


    validateSliderClose() {
        setTimeout(() => {
            this.closePage.emit();
        }, 500);
    }

    loadProjectOverviewCommentBody(): void {
        this.commentFetchRequestPayload = new projectOverviewCommentFetch();
        this.commentFetchRequestPayload.commentTypeCode = '1';
        this.commentFetchRequestPayload.moduleCode = '3';
        this.commentFetchRequestPayload.moduleItemKey = this.dataForCommentSlider?.projectDetails?.projectId;
        this.getProjectOverviewComments(this.commentFetchRequestPayload);
    }

    getProjectOverviewComments(commentFetchRequestPayload: any): void {
        this.$subscriptions.push(this.projectOverviewService.getProjectOverviewComments(commentFetchRequestPayload).subscribe((res: any) => {
            this.commentsData = res;
            this.currentUserId = this.commonService.getCurrentUserDetail('personID');
            this.initializeVisibleComments();
        }));
    }


    addComment(details): void {
        this.$subscriptions.push(this.projectOverviewService.addProjectOverviewComment(details).subscribe((res: any) => {
            this.cancelOrClearCommentsDetails();
            if (details) {
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Comment added successfully');
                this.dataForCommentSlider.projectDetails.commentCount++;
                this.commentCountUpdated.emit(this.dataForCommentSlider.projectDetails.commentCount);
            }
            this.showAddComment = false;
        }, error => {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    updateComment(details): void {
        this.$subscriptions.push(this.projectOverviewService.updateProjectOverviewComment(details).subscribe((res: any) => {
            this.cancelOrClearCommentsDetails();
            if (details) {
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Comment updated successfully');
            }
        }, error => {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }))
    }

    deleteComment(details): void {
        this.$subscriptions.push(this.projectOverviewService.deleteProjectOverviewComments(details.commentId).subscribe((res: any) => {
            this.cancelOrClearCommentsDetails();
            this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Comment deleted successfully');
            this.dataForCommentSlider.projectDetails.commentCount--;
            this.commentCountUpdated.emit(this.dataForCommentSlider.projectDetails.commentCount)
        }, error => {
            if (error.status === 405) {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Unable to delete the comment');
            } else {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }
        }))
    }

    toggleAddCommentBox(): void {
        this.showAddComment = !this.showAddComment;
    }

    public onReady(editor) {
        editor.ui.getEditableElement().parentElement.insertBefore(
            editor.ui.view.toolbar.element,
            editor.ui.getEditableElement()
        );
    }

    replyComment(commentDetails, index): void {
        if (!this.isReplyComment && !this.projectOverviewService.isEditParentComment && !this.isEditComment) {
            this.projectOverviewCommentDetails.parentCommentId = commentDetails.commentId;
            this.isReplyComment = true;
        }
    }

    addReplayCommentsDetails(): void {
        if (!this.validateComment()) {
            this.addCommentsDetails()
        }
    }

    addCommentsDetails(): void {
        if (!this.validateComment()) {
            this.projectOverviewCommentDetails.commentTypeCode = '1';
            this.projectOverviewCommentDetails.moduleCode = '3';
            this.projectOverviewCommentDetails.moduleItemKey = this.dataForCommentSlider?.projectDetails?.projectId;
            this.addComment(this.projectOverviewCommentDetails)
        }
    }

    editParentComment(details): void {
        if (!this.projectOverviewService.isEditParentComment && !this.isEditComment && !this.isReplyComment) {
            this.projectOverviewService.isEditParentComment = true;
            this.projectOverviewService.editParentCommentId = details.commentId;
        }
    }

    addEditedComment(details): void {
        this.isEditComment = true;
        this.projectOverviewCommentDetails.commentId = details.commentId;
        this.projectOverviewCommentDetails.comment = details.comment;
        if (!this.validateComment()) {
            this.updateComment(this.projectOverviewCommentDetails)
        }
    }
    
    validateComment(): boolean {
        this.mandatoryMap.clear();
        if (!this.projectOverviewCommentDetails.comment) {
            this.mandatoryMap.set('comment', 'Please add comment');
        }
        return this.mandatoryMap.size > 0;
    }

    cancelOrClearCommentsDetails(): void {
        this.projectOverviewCommentDetails = new CoiProjectOverviewComment();
        this.isEditComment = false;
        this.isReplyComment = false;
        this.projectOverviewService.isEditParentComment = false;
        this.loadProjectOverviewCommentBody();
        this.projectOverviewService.editParentCommentId = null;
        this.mandatoryMap.clear();
    }

    initializeVisibleComments(): void {
        if (this.commentsData?.length) {
            this.commentsData.forEach((comment, index) => {
                if (comment.childComments) {
                    this.visibleCommentsMap[index] = comment.childComments.slice(0, this.initialVisibleComments);
                } else {
                    this.visibleCommentsMap[index] = [];
                }
            });
        }
    }

    viewMore(replyIndex: number): void {
        if (this.commentsData[replyIndex]?.childComments) {
            this.visibleCommentsMap[replyIndex] = this.commentsData[replyIndex].childComments;
        }
    }

    viewLess(replyIndex: number): void {
        if (this.commentsData[replyIndex]?.childComments) {
            this.visibleCommentsMap[replyIndex] = this.commentsData[replyIndex].childComments.slice(0, this.initialVisibleComments);
        }
    }

    editReplyComment(childComment, index): void {
        if (!this.isEditComment && !this.projectOverviewService.isEditParentComment && !this.isReplyComment) {
            this.isEditComment = true;
            this.currentEditIndex = index;
            this.projectOverviewCommentDetails.commentId = childComment.commentId;
        }
    }

    addReplyEditedComments(childComment): void {
        this.projectOverviewCommentDetails.comment = childComment.comment;
        if (!this.validateComment()) {
            this.updateComment(this.projectOverviewCommentDetails)
        }
    }  
}
