import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { EntityDetailsService } from '../entity-details.service';
import { EntityDetail } from '../../sfi/add-sfi.interface';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { deepCloneObject, hideModal, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
  selector: 'app-entity-questionnaire',
  templateUrl: './entity-questionnaire.component.html',
  styleUrls: ['./entity-questionnaire.component.scss']
})
export class EntityQuestionnaireComponent implements OnInit, OnDestroy, OnChanges {

  $externalSaveEvent = new BehaviorSubject<Boolean>(null);
  configuration: any = {
    moduleItemCode: 8,
    moduleSubitemCodes: [801],
    moduleItemKey: '',
    moduleSubItemKey: '',
    actionUserId: this._commonService.getCurrentUserDetail('personId'),
    actionPersonName: this._commonService.getCurrentUserDetail('fullName'),
    enableViewMode: false,
    isChangeWarning: true,
    isEnableVersion: true,
  };
  isAddRelationButtonToggled = false;
  currentSelected = {
    tab: 'FINANCIAL'
  };
  isShowRelationshipModal = false;
  coiFinancialEntityDetail: EntityDetail = new EntityDetail();
  relationValidationMap = new Map();
  $subscriptions: Subscription[] = [];
  @Output() updateRelationship: EventEmitter<any> = new EventEmitter<any>();
  @Input() isEditMode = false;
  @Output() positionsToView: EventEmitter<boolean> = new EventEmitter<boolean>();
  @Input() entityId: any;
  currentRelationshipDetails: any = {};
  isHoverAddRelationship = false;
  @Input() isSwitchCurrentTab = false;
  @Output() deleteRelationshipEvent: EventEmitter<any> = new EventEmitter<any>();
  isConcurrency = false;

  constructor(private _commonService: CommonService, private _router: Router,
    public entityDetailsServices: EntityDetailsService,
    private _activatedRoute: ActivatedRoute
  ) { }

  ngOnInit() {
    this.$subscriptions.push(this._activatedRoute.queryParams.subscribe(params => {
      this.isEditMode = this._activatedRoute.snapshot.queryParamMap.get('mode') === 'edit';
      this.configuration.enableViewMode = !this.isEditMode;
      this.getQuestionnaire(this.entityDetailsServices.currentRelationshipQuestionnaire);
      this.getDefinedRelationships();
    }));
    this.openRelationshipQuestionnaire();
  }

  ngOnChanges() {
    this.configuration.enableViewMode = !this.isEditMode;
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  async getRelationshipLookUp(): Promise<any> {
    try {
      const response = await this.entityDetailsServices.addSFILookUp();
      return response;
    } catch (error) {
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }
  }

  addRelations(flag = false) {
    this.isAddRelationButtonToggled = flag;
  }

  navigateBack() {
    this._router.navigateByUrl(this.entityDetailsServices.previousURL);
  }

  getDefinedRelationships() {
        (this.isEditMode && this.entityDetailsServices.definedRelationships.length > 0) ? this.positionsToView.emit(true) : this.positionsToView.emit(false);
  }

  getQuestionnaire(data: any) {
    if(data) {
      this.currentRelationshipDetails = data;
      this.entityDetailsServices.activeRelationship = data.validPersonEntityRelType.validPersonEntityRelTypeCode;
      this.entityDetailsServices.clickedTab = 'QUESTIONNAIRE';
      this.configuration.moduleItemKey = this._activatedRoute.snapshot.queryParamMap.get('personEntityId') || this.entityId;
      this.configuration.moduleSubItemKey = data.validPersonEntityRelTypeCode;
      this.configuration = Object.assign({}, this.configuration);
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
    this.entityDetailsServices.$emitUnsavedChangesModal.next({ details : data, isLeaveFromRelationTab : true });
  }

  questionnaireSaveAction(event) {
    this.entityDetailsServices.$saveQuestionnaireAction.next(event);
  }

  questionnaireEdit(event) {
    if(event) {
      this.entityDetailsServices.isRelationshipQuestionnaireChanged = true;
      let nameOfQuestionnaire = this.entityDetailsServices.definedRelationships.find(ele => ele.validPersonEntityRelType.validPersonEntityRelTypeCode == this.entityDetailsServices.activeRelationship);
      if(!this.entityDetailsServices.unSavedSections.some(ele => ele.includes('Relationship Questionnaire'))) {
        this.entityDetailsServices.unSavedSections.push( nameOfQuestionnaire.validPersonEntityRelType.description +' Relationship Questionnaire');
      }
    }
  }

  deleteRelationship() {
    let removeRelId = this.currentRelationshipDetails.personEntityRelId;
    let VALID_REL_TYPE_CODE = this.currentRelationshipDetails.validPersonEntityRelTypeCode;
    this.$subscriptions.push(this.entityDetailsServices.deletePersonEntityRelationship
      (this.currentRelationshipDetails.personEntityRelId, this.currentRelationshipDetails.personEntityId).subscribe(async (updatedTimestamp) => {
        this.updateDefinedRelationships();
        if(VALID_REL_TYPE_CODE in this.entityDetailsServices.relationshipCompletedObject) {
          delete this.entityDetailsServices.relationshipCompletedObject[VALID_REL_TYPE_CODE];
        }
        this.deleteRelationshipEvent.emit({'updatedTimestamp': updatedTimestamp,'removeRelId': removeRelId, 'isDeleted': true}); 
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationship deleted successfully.');
      }, _err => {
        if (_err.status === 405) {
          this.entityDetailsServices.concurrentUpdateAction = 'Delete Relationship'
      } else {
          this._commonService.showToast(HTTP_ERROR_STATUS, `Error in deleting relationship.`);
      }
      }));
  }

  async updateDefinedRelationships() {
    this.entityDetailsServices.availableRelationships = await this.getRelationshipLookUp();
    let delIndex = this.entityDetailsServices.definedRelationships.findIndex(ele => ele.personEntityRelId === this.currentRelationshipDetails.personEntityRelId);
    if(delIndex > -1) {
      this.entityDetailsServices.definedRelationships.splice(delIndex, 1);
    }
    if(!this.entityDetailsServices.definedRelationships.length) {
      this.entityDetailsServices.selectedTab = 'RELATIONSHIP_DETAILS';
    }
    if(this.entityDetailsServices.definedRelationships.length) {
      this.getQuestionnaire(this.entityDetailsServices.definedRelationships[0]);
    }
    let index = this.entityDetailsServices.unSavedSections.findIndex(ele => ele.includes('Relationship Questionnaire'));
    if (index >= 0) {
        this.entityDetailsServices.unSavedSections.splice(index, 1);
    }
    this.entityDetailsServices.isRelationshipQuestionnaireChanged = false;
  }

}
