import { Component, OnInit, Input, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { ActivityService } from './activity.service';
import { DataStoreService } from '../services/data-store.service';
import { CoiService } from '../services/coi.service';
import { CommentConfiguration, CommentRequest } from '../coi-interface';
import {ElasticConfigService} from "../../../../../fibi/src/app/common/services/elastic-config.service";
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";

declare let $: any;
@Component({
    selector: 'app-coi-review-comment-modal',
    templateUrl: './activity.component.html',
    styleUrls: ['./activity.component.css'],
    providers: [ActivityService, ElasticConfigService]
})
export class ActivityComponent implements OnInit, OnDestroy {

    @Input() coiSectionsType = [];
    @Input() adminGroupList = [];

    subSectionList: any = [];
    editIndex: number = -1;

    $subscriptions: Subscription[] = [];

    reviewCommentObject: CommentRequest = new CommentRequest();
    validationMap = new Map();
    modalConfiguration: CommentConfiguration = new CommentConfiguration();
    personElasticOptions: any = {};
    assigneeClearField: String;
    coiSection: any = [];
    coiDisclosure: any = {};
    adminGroupsCompleterOptions: any = {};
    categoryClearFiled: String;
    uploadedFile: any = [];
    adminGroup: any = [];

    commentTab = 'MODIFY';

    constructor(
        public _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _coiService: CoiService,
        private _elasticConfigService: ElasticConfigService,
        private _reviewService: ActivityService
    ) { }

    ngOnInit() {
        this.getDataFromStore();
        this.listenTriggerReviewComment();
        this.personElasticOptions = this._elasticConfigService.getElasticForPerson();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore() {
        const DATA = this._dataStore.getData(['coiSectionsType', 'coiDisclosure', 'adminGroup']);
        this.coiSection = this.coiSectionsType.length ? this.coiSectionsType : DATA.coiSectionsType;
        this.coiDisclosure = DATA.coiDisclosure;
        this.adminGroup = this.adminGroupList.length ? this.adminGroupList : DATA.adminGroup;
        if (this.adminGroup) {
            this.setAdminGroupOptions();
        }
    }

    private setAdminGroupOptions(): void {
        this.adminGroupsCompleterOptions = {
            arrayList: this.getActiveAdminGroups(),
            contextField: 'adminGroupName',
            filterFields: 'adminGroupName',
            formatString: 'adminGroupName',
            defaultValue: ''
        };
    }

    private getActiveAdminGroups(): any[] {
        return this.adminGroup.filter(element => element.isActive === 'Y');
    }

    private listenTriggerReviewComment() {
        this.$subscriptions.push(this._coiService.triggerAddReviewComment$.subscribe((data: any) => {
            if (data) {
                this.modalConfiguration = JSON.parse(JSON.stringify(data));
                this.setReviewCommentObject();
                this.subSectionList = this.modalConfiguration.subSectionList;
                this.editIndex = this.modalConfiguration.modifyIndex;
                $('#coi-activity-modal').modal('show');
                this.commentTab = 'MODIFY';
            }
        }));
    }

    setReviewCommentObject() {
        this.reviewCommentObject.coiSectionsTypeCode = this.modalConfiguration.coiSectionsTypeCode;
        this.reviewCommentObject.coiParentCommentId = this.modalConfiguration.coiParentCommentId;
        this.reviewCommentObject.comment = this.modalConfiguration.comment;
        this.reviewCommentObject.disclosureId = this.modalConfiguration.disclosureId;
        this.reviewCommentObject.isPrivate = this.modalConfiguration.isPrivate;
        this.reviewCommentObject.coiReviewCommentTag = this.modalConfiguration.coiReviewCommentTag;
        this.reviewCommentObject.coiReviewCommentAttachment = this.modalConfiguration.coiReviewCommentAttachment;
        this.reviewCommentObject.coiReviewId = this.modalConfiguration.coiReviewId;
        this.reviewCommentObject.coiReviewCommentId = this.modalConfiguration.coiReviewCommentId;
        this.reviewCommentObject.coiSubSectionsId = this.modalConfiguration.isSubSectionComment ?
            this.modalConfiguration.coiSubSectionsId : null;
    }

    private coiReviewCommentTag(person: any, group: any) {
        return {
            tagRef: null,
            tagPersonId: person ? person.prncpl_id : null,
            tagPersonFullName: person ? person.full_name : null,
            tagGroupId: group ? group.adminGroupId : null,
            tagGroupName: group ? group.adminGroupName : null
        };
    }

    selectPerson(event: any) {
        if (event) {
            this.reviewCommentObject.coiReviewCommentTag.push(this.coiReviewCommentTag(event, null));
        }
        this.personElasticOptions = this._elasticConfigService.getElasticForPerson();
    }

    adminGroupSelect(event: any) {
        if (event) {
            this.reviewCommentObject.coiReviewCommentTag.push(this.coiReviewCommentTag(null, event));
        }
        this.setAdminGroupOptions();
    }

    removeReviewer(index: number) {
        this.reviewCommentObject.coiReviewCommentTag.splice(index, 1);
    }

    addAttachments(files: any) {
        if (files && files.length) {
            Array.from(files).forEach((element: any) => {
                this.uploadedFile.push(element);
            });
        }
    }

    clearModal() {
        $('#coi-activity-modal').modal('hide');
        this.uploadedFile = [];
        this.reviewCommentObject = new CommentRequest();
    }

    private findSection(sectionCode: string) {
        return this.coiSection.find(ele => ele.coiSectionsTypeCode == sectionCode);
    }

    private findSubSection(coiSubSectionsId: string) {
        const SUB_SECTION = this.subSectionList.find(ele => ele.coiSubSectionsCode == coiSubSectionsId);
        const SUB_SECTION_KEY = SUB_SECTION.coiSectionsTypeCode === '2' ? 'coiFinancialEntity' : 'coiDisclosureDetails';
        this.reviewCommentObject[SUB_SECTION_KEY] = SUB_SECTION[SUB_SECTION_KEY];
    }

    addComments() {
        if (this.validateComment()) {
            this.reviewCommentObject.coiSectionsType = this.findSection(this.reviewCommentObject.coiSectionsTypeCode);
            if (this.modalConfiguration.isSubSectionComment) {
                this.findSubSection(this.reviewCommentObject.coiSubSectionsId);
            }
            this.$subscriptions.push(this._reviewService.addCOIReviewComment({
                coiReviewComment: this.reviewCommentObject
            }, this.uploadedFile).subscribe((data: any) => {
                this._coiService.triggerReviewCommentDataUpdate$.next({
                    coiReviewComment: {
                        ...data.coiReviewComment,
                        coiReviewCommentAttachment: data.coiReviewCommentAttachment
                    },
                    commentType: this.setAddCommentType(),
                    coiParentCommentId: this.modalConfiguration.coiParentCommentId,
                    modifyIndex: this.modalConfiguration.modifyIndex
                });
                this.clearModal();
                // this._commonService.showToast(HTTP_SUCCESS_STATUS,
                //     `Review comment ${ this.modalConfiguration.modifyIndex === -1 ? 'added' : 'updated'} successfully.`);
            }, _err => {
                // this._commonService.showToast(HTTP_ERROR_STATUS,
                //     `Error in adding ${ this.modalConfiguration.modifyIndex === -1 ? 'adding' : 'updating'}  comment. Please try again.`);
            }));
        }
    }

    private setAddCommentType() {
        if (this.modalConfiguration.coiParentCommentId) {
            return this.modalConfiguration.modifyIndex !== -1 ? 'MODIFY_REPLY' : 'ADD_REPLY';
        } else {
            return this.modalConfiguration.modifyIndex !== -1 ? 'MODIFY_COMMENT' : 'ADD_COMMENT';
        }
    }

    validateComment() {
        this.validationMap.clear();
        if (!this.reviewCommentObject.coiSectionsTypeCode || this.reviewCommentObject.coiSectionsTypeCode === 'null') {
            this.validationMap.set('coiSectionsTypeCode', 'Please select a section type.');
        }
        if (!this.reviewCommentObject.comment) {
            this.validationMap.set('comment', 'Please add a comment.');
        }
        return this.validationMap.size === 0;
    }

}
