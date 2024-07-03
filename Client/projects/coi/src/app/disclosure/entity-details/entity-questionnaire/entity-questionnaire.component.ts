import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output, SimpleChanges } from '@angular/core';
import { BehaviorSubject, Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { EntityDetailsService, groupBy } from '../entity-details.service';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { deepCloneObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-entity-questionnaire',
    templateUrl: './entity-questionnaire.component.html',
    styleUrls: ['./entity-questionnaire.component.scss']
})
export class EntityQuestionnaireComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    @Input() isEditMode;
    @Input() relationshipDetails;
    @Input() entityId: any;
    @Output() deleteRelationshipEvent: EventEmitter<any> = new EventEmitter<any>();
    @Output() positionsToView: EventEmitter<boolean> = new EventEmitter<boolean>();
    configuration: any = {
        moduleItemCode: 8, //8 - COI disclosure module code
        moduleSubitemCodes: [801], //801 - COI sfi code
        moduleItemKey: '',
        moduleSubItemKey: '',
        actionUserId: this._commonService.getCurrentUserDetail('personID'),
        actionPersonName: this._commonService.getCurrentUserDetail('fullName'),
        enableViewMode: false,
        isChangeWarning: true,
        isEnableVersion: true,
    };
    $externalSaveEvent = new BehaviorSubject<Boolean>(null);
    currentRelationshipDetails: any = {};
    hasPermissionToView = true;
    deleteHelpText = 'You are about to delete entity relationship.';

    constructor(private _commonService: CommonService, public entityDetailsServices: EntityDetailsService) { }

    ngOnInit() {
        this.openRelationshipQuestionnaire();
    }

    ngOnChanges() {
        this.configuration.enableViewMode = !this.isEditMode;
        this.canScrollToPosition();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    canScrollToPosition() {
        (this.isEditMode && this.entityDetailsServices.definedRelationships.length > 0) ? this.positionsToView.emit(true) : this.positionsToView.emit(false);
    }

    getQuestionnaire(data: any) {
        this.currentRelationshipDetails = {};
        if (data) {
            this.entityDetailsServices.activeRelationship = data.validPersonEntityRelType.validPersonEntityRelTypeCode;
            this.entityDetailsServices.toBeActiveTab = 'QUESTIONNAIRE';
            this.currentRelationshipDetails = data;
            if ((this.relationshipDetails && this.relationshipDetails.personId === this._commonService.getCurrentUserDetail('personID')) || this.hasRightToView(data.validPersonEntityRelType.disclosureTypeCode)) {
                this.hasPermissionToView = true;
                this.configuration.moduleItemKey = this.entityId;
                this.configuration.moduleSubItemKey = data.validPersonEntityRelTypeCode;
                this.configuration = Object.assign({}, this.configuration);
            } else {
                this.hasPermissionToView = false;
            }
        }
    }

    hasRightToView(disclosureTypeCode) {
        switch (disclosureTypeCode) {
            case '1':
                return this._commonService.getAvailableRight(['VIEW_FCOI_DISCLOSURE', 'MANAGE_FCOI_DISCLOSURE',
                    'VIEW_PROJECT_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE']);
            case '2':
                return this._commonService.getAvailableRight(['VIEW_OPA_DISCLOSURE', 'MANAGE_OPA_DISCLOSURE']);
            case '3':
                return this._commonService.getAvailableRight(['VIEW_TRAVEL_DISCLOSURE', 'MANAGE_TRAVEL_DISCLOSURE']);
        }
    }

    openRelationshipQuestionnaire() {
        this.$subscriptions.push(this.entityDetailsServices.$openQuestionnaire.subscribe((data: any) => {
            if (data) {
                this.entityDetailsServices.isRelationshipQuestionnaireChanged ? this.leaveCurrentRelationship(data) : this.getQuestionnaire(data);
            }
        }));
    }

    leaveCurrentRelationship(data: any) {
        this.entityDetailsServices.$emitUnsavedChangesModal.next({ details: data, isLeaveFromRelationTab: true });
    }

    questionnaireSaveAction(event) {
        this.entityDetailsServices.$saveQuestionnaireAction.next(event);
    }

    questionnaireEdit(event) {
        if (event) {
            this.entityDetailsServices.isRelationshipQuestionnaireChanged = true;
            let nameOfQuestionnaire = this.entityDetailsServices.definedRelationships.find(ele => ele.validPersonEntityRelType.validPersonEntityRelTypeCode == this.entityDetailsServices.activeRelationship);
            if (!this.entityDetailsServices.unSavedSections.some(ele => ele.includes('Relationship Questionnaire'))) {
                this.entityDetailsServices.unSavedSections.push(nameOfQuestionnaire.validPersonEntityRelType.description + ' Relationship Questionnaire');
            }
        }
    }

    deleteRelationship() {
        this.$subscriptions.push(this.entityDetailsServices.deletePersonEntityRelationship
            (this.currentRelationshipDetails.personEntityRelId, this.currentRelationshipDetails.personEntityId).subscribe(async (res: any) => {
                this.updateDefinedRelationships(res);
                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationship deleted successfully.');
            }, _err => {
                if (_err.status === 405) {
                    this.entityDetailsServices.concurrentUpdateAction = 'Delete Relationship'
                } else {
                    this._commonService.showToast(HTTP_ERROR_STATUS, `Error in deleting relationship.`);
                }
            }));
    }

    async updateDefinedRelationships(res) {
        await this.addToAvailableRelation();
        this.removeUnsavedDetailsChanges();
        if (this.currentRelationshipDetails.validPersonEntityRelTypeCode in this.entityDetailsServices.relationshipCompletedObject) {
            delete this.entityDetailsServices.relationshipCompletedObject[this.currentRelationshipDetails.validPersonEntityRelTypeCode];
        }
        this.entityDetailsServices.$addOrDeleteRelation.next({ 'deletedId': this.currentRelationshipDetails.personEntityRelId, 'isFormCompleted': res.isFormCompleted ,'updateTimestamp' : res.updateTimestamp });
        this.entityDetailsServices.isRelationshipQuestionnaireChanged = false;
    }

    removeUnsavedDetailsChanges() {
        let delIndex = this.entityDetailsServices.definedRelationships.findIndex(ele => ele.personEntityRelId === this.currentRelationshipDetails.personEntityRelId);
        if (delIndex > -1) {
            this.entityDetailsServices.definedRelationships.splice(delIndex, 1);
        }
        if (this.entityDetailsServices.definedRelationships.length) {
            this.getQuestionnaire(this.entityDetailsServices.definedRelationships[0]);
            this.entityDetailsServices.currentRelationshipQuestionnaire = this.entityDetailsServices.definedRelationships[0];
        } else {
            this.entityDetailsServices.currentRelationshipQuestionnaire = {};
        }
        let index = this.entityDetailsServices.unSavedSections.findIndex(ele => ele.includes('Relationship Questionnaire'));
        if (index >= 0) {
            this.entityDetailsServices.unSavedSections.splice(index, 1);
        }
    }

    async addToAvailableRelation() {
        let relationIndex = this.entityDetailsServices.allAvailableRelationships.findIndex(ele => ele.validPersonEntityRelTypeCode == this.currentRelationshipDetails.validPersonEntityRelTypeCode);
        this.entityDetailsServices.groupedRelations = {};
        if (this.entityDetailsServices.remainingRelationships.length && this.entityDetailsServices.remainingRelationships[relationIndex] && this.entityDetailsServices.remainingRelationships[relationIndex].validPersonEntityRelTypeCode == this.currentRelationshipDetails.validPersonEntityRelTypeCode) {
            this.entityDetailsServices.remainingRelationships.splice(relationIndex, 1);
        }
        this.entityDetailsServices.remainingRelationships.splice(relationIndex, 0, this.currentRelationshipDetails.validPersonEntityRelType);
        if (this.entityDetailsServices.remainingRelationships.length) {
            this.entityDetailsServices.groupedRelations = groupBy(deepCloneObject(this.entityDetailsServices.remainingRelationships), "coiDisclosureType", "description");
        }
    }

}
