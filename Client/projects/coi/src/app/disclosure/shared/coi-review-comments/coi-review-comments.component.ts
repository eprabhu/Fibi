import { Component, Input, OnChanges, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { CommentConfiguration } from '../../coi-interface';
import { CoiService } from '../../services/coi.service';
import { DataStoreService } from '../../services/data-store.service';
import { CoiReviewCommentsService } from './coi-review-comments.service';
import {ElasticConfigService} from "../../../../../../fibi/src/app/common/services/elastic-config.service";
import {subscriptionHandler} from "../../../../../../fibi/src/app/common/utilities/subscription-handler";
import {fileDownloader} from "../../../../../../fibi/src/app/common/utilities/custom-utilities";

@Component({
    selector: 'app-coi-review-comments',
    templateUrl: './coi-review-comments.component.html',
    styleUrls: ['./coi-review-comments.component.css'],
    providers: [CoiReviewCommentsService]
})
export class CoiReviewCommentsComponent implements OnInit, OnDestroy, OnChanges {

    @Input() disclosureId: string = null;
    @Input() sectionCode: string = null;
    @Input() subSectionId: string = null;
    @Input() personId: string = null;
    @Input() sortOrder = 'asec';
    @Input() isAllowModification = true;
    @Input() isShowDisclosureInfo = false;
    @Input() isShowFilters = false;

    $subscriptions: Subscription[] = [];
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    reviewComment: any = [];
    deleteComment: any = {};
    deleteCommentAttachment: any = {};
    coiSection: any = [];
    adminGroup: any = [];
    personElasticOptions: any = {};
    assigneeClearField: String;
    filterObject: any = {};

    constructor(
        private _coiService: CoiService,
        private _commentService: CoiReviewCommentsService,
        private _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _elasticConfigService: ElasticConfigService
    ) { }

    ngOnInit() {
        this.listenToCommentsUpdate();
        this.getDataFromStore();
    }

    ngOnChanges() {
        this.loadCoiReviewComments();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore() {
        const DATA = this._dataStore.getData(['coiSectionsType']);
        this.coiSection = DATA.coiSectionsType;
        this.personElasticOptions = this._elasticConfigService.getElasticForPerson();
    }

    selectPerson(event: any) {
        this.filterObject.personId = event ? event.prncpl_id : null;
    }

    clearFiler() {
        this.filterObject = {};
        this.assigneeClearField = new String('true');
        this.loadCoiReviewComments();
    }

    loadCoiReviewComments() {
        this.$subscriptions.push(this._commentService.loadCoiReviewComments({
            personId: this.filterObject.personId ? this.filterObject.personId : this.personId,
            disclosureId: this.disclosureId,
            coiSubSectionsId: this.subSectionId,
            coiSectionsTypeCode: this.filterObject.sectionCode ? this.filterObject.sectionCode : this.sectionCode,
            sort : this.sortOrder
        }).subscribe((data: any) => {
            this.reviewComment = this.setCommentArray(data.coiReviewComments);
        }, _err => {
            // this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in loading review comments. Please try again later.');
        }));
    }

    setCommentArray(commentArray: any) {
        const COMMENT_ARRAY = [];
        commentArray.forEach(comment => {
            if (!comment.coiParentCommentId) {
                const COMMENT = JSON.parse(JSON.stringify(comment));
                COMMENT.childComments = this.filterChildComments(comment.coiReviewCommentId, commentArray);
                COMMENT.isShowReplies = false;
                COMMENT_ARRAY.push(COMMENT);
            }
        });
        return COMMENT_ARRAY;
    }

    filterChildComments(parentCommentId: string, commentArray: any) {
        return commentArray.filter(comment => comment.coiParentCommentId === parentCommentId);
    }

    private listenToCommentsUpdate() {
        this.$subscriptions.push(
            this._coiService.triggerReviewCommentDataUpdate$.subscribe((data: any) => {
                this.updateCommentDetails(data);
            })
        );
    }

    private updateCommentDetails(data: any) {
        switch (data.commentType) {
            case 'ADD_COMMENT': this.addComment(this.reviewComment, data.coiReviewComment); break;
            case 'MODIFY_COMMENT': this.updateComment(this.reviewComment, data.coiReviewComment, data.modifyIndex); break;
            case 'ADD_REPLY': {
                const PARENT_COMMENT = this.findParentComment(data.coiParentCommentId);
                PARENT_COMMENT.isShowReplies = true;
                PARENT_COMMENT.childComments = PARENT_COMMENT.childComments ? PARENT_COMMENT.childComments : [];
                this.addComment(PARENT_COMMENT.childComments, data.coiReviewComment); break;
            }
            case 'MODIFY_REPLY': {
                const PARENT_COMMENT = this.findParentComment(data.coiParentCommentId);
                PARENT_COMMENT.isShowReplies = true;
                this.updateComment(PARENT_COMMENT.childComments, data.coiReviewComment, data.modifyIndex); break;
            }
        }
    }

    private findParentComment(coiParentCommentId) {
        return this.reviewComment.find(comment => comment.coiReviewCommentId === coiParentCommentId);
    }

    private addComment(commentArray: any, newComment: any) {
        commentArray.push(newComment);
    }

    private updateComment(commentArray: any, newComment: any, index: number) {
        const CHILD_COMMENTS = JSON.parse(JSON.stringify(commentArray[index].childComments));
        if (CHILD_COMMENTS.length) {
            newComment.childComments = CHILD_COMMENTS;
        }
        commentArray.splice(index, 1, newComment);
    }

    downloadAttachment(attachment: any) {
        this.$subscriptions.push(this._commentService.downloadCoiReviewAttachment(attachment.coiReviewCommentAttId)
            .subscribe((data: any) => {
                fileDownloader(data, attachment.fileName);
            }, _err => {
               // this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in downloading attachment. Please try again later.');
            }));
    }

    modifyReviewComment(comment: any, index: number, isEdit: boolean, coiParentCommentId: number = null) {
        this.commentConfiguration.disclosureId = comment.coiDisclosure.disclosureId;
        this.commentConfiguration.coiSectionsTypeCode = comment.coiSectionsTypeCode;
        this.commentConfiguration.coiReviewId = comment.coiReviewId;
        this.commentConfiguration.coiParentCommentId = coiParentCommentId;
        if (isEdit) {
            this.editComment(comment, index);
        }
        this._coiService.triggerCommentModal(this.commentConfiguration);
    }

    editComment(comment: any, index: number) {
        this.commentConfiguration.modifyIndex = index;
        this.commentConfiguration.coiReviewCommentId = comment.coiReviewCommentId;
        this.commentConfiguration.comment = comment.comment;
        this.commentConfiguration.coiReviewCommentAttachment = comment.coiReviewCommentAttachment;
        this.commentConfiguration.isPrivate = comment.isPrivate;
        this.commentConfiguration.coiReviewCommentTag = comment.coiReviewCommentTag;
    }

    deleteReviewComment() {
        this.$subscriptions.push(this._commentService.deleteReviewComment(this.deleteComment.coiReviewCommentId)
            .subscribe((_data: any) => {
                if (this.deleteComment.coiParentCommentId) {
                    const PARENT_COMMENT = this.findParentComment(this.deleteComment.coiParentCommentId);
                    this.deleteFromArray(PARENT_COMMENT, this.deleteComment.index);
                } else {
                    this.deleteFromArray(this.reviewComment, this.deleteComment.index);
                }
                this.deleteComment = {};
                // this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Comment deleted successfully.');
            }, _err => {
                // this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in deleting comment. Please try again later.');
            }));
    }

    setDeleteObject(comment: any, index: number, coiReviewCommentAttId: number = null): void {
        this.deleteComment.index = index;
        this.deleteComment.coiReviewCommentId = comment.coiReviewCommentId;
        this.deleteComment.coiParentCommentId = comment.coiParentCommentId;
        this.deleteComment.coiReviewCommentAttId = coiReviewCommentAttId;
    }

    private deleteFromArray(array: any, index: number) {
        array.splice(index, 1);
    }

    deleteReviewAttachment() {
        this.$subscriptions.push(this._commentService.deleteReviewAttachment(this.deleteComment.coiReviewCommentAttId)
            .subscribe((_data: any) => {
                if (this.deleteComment.coiParentCommentId) {
                    const PARENT_COMMENT = this.findParentComment(this.deleteComment.coiParentCommentId);
                    this.findAndDeleteAttachment(PARENT_COMMENT.childComments, this.deleteComment.coiReviewCommentId);
                } else {
                    this.findAndDeleteAttachment(this.reviewComment, this.deleteComment.coiReviewCommentId);
                }
                // this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Comment attachment deleted successfully.');
                this.deleteComment = {};
            }, _err => {
                // this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in deleting attachment. Please try again later.');
            }));
    }

    private findAndDeleteAttachment(commentArray: any, coiReviewCommentId: any) {
        const COMMENT = commentArray.find(comment => comment.coiReviewCommentId === coiReviewCommentId);
        if (COMMENT.coiReviewCommentAttachment && COMMENT.coiReviewCommentAttachment.length) {
            this.deleteFromArray(COMMENT.coiReviewCommentAttachment, this.deleteComment.index);
        }
    }

    getSubSectionDescription(comment: any) {
        return comment.coiSectionsTypeCode === '2' ? comment.coiFinancialEntity.coiEntity.coiEntityName :
            comment.coiDisclosureDetails.coiFinancialEntity.coiEntity.coiEntityName;
    }

}
