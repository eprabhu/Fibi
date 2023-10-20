import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { EntityDetailsService } from '../entity-details.service';
import { EntityDetail } from '../../sfi/add-sfi.interface';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { hideModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';

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
  availableRelationships: any = [];
  definedRelationships: any = [];
  isAddRelationButtonToggled = false;
  activeRelationship: any = 0;
  currentSelected = {
    tab: 'FINANCIAL'
  };
  isShowRelationshipModal = false;
  coiFinancialEntityDetail: EntityDetail = new EntityDetail();
  isSaving = false;
  relationValidationMap = new Map();
  $subscriptions: Subscription[] = [];
  @Output() updateRelationship: EventEmitter<any> = new EventEmitter<any>();
  @Input() isEditMode = false;
  @Output() positionsToView: EventEmitter<boolean> = new EventEmitter<boolean>();
  @Input() entityId: any;
  isChecked = {};
  currentRelationshipDetails: any = {};
  isHoverAddRelationship = false;
  @Output() emitLeaveModal: EventEmitter<any> = new EventEmitter<any>();
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
      this.getDataFromService();
      this.configuration.enableViewMode = !this.isEditMode;
    }));
  }

  ngOnChanges() {
    this.configuration.enableViewMode = !this.isEditMode;
    if (this.isSwitchCurrentTab) {
      this.leaveCurrentTab();
    }
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  async getDataFromService() {
    this.availableRelationships = await this.getRelationshipLookUp();
    await this.getDefinedRelationships();
    if (this.definedRelationships.length > 0) {
      this.getQuestionnaire(this.definedRelationships[0]);
    }
    this.removeExistingRelation();
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
    const REQ_BODY = {
      'personEntityId': this._activatedRoute.snapshot.queryParamMap.get('personEntityId') || this.entityId
    };
    return new Promise<boolean>((resolve) => {
      this.$subscriptions.push(this.entityDetailsServices.getPersonEntityRelationship(REQ_BODY).subscribe((res: any) => {
        this.configuration.moduleItemKey = this._activatedRoute.snapshot.queryParamMap.get('personEntityId') || this.entityId;
        this.definedRelationships = res || [];
        (this.isEditMode && this.definedRelationships.length > 0) ? this.positionsToView.emit(true) : this.positionsToView.emit(false);
        this.removeExistingRelation();
        resolve(true);
      }, error => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
      }));
    });
  }

  getQuestionnaire(data: any) {
    if(data) {
      this.currentRelationshipDetails = data;
      this.activeRelationship = data.validPersonEntityRelType.personEntityRelType.relationshipTypeCode;
      this.configuration.moduleSubItemKey = data.validPersonEntityRelTypeCode;
      this.configuration = Object.assign({}, this.configuration);
    }
  }

  openRelationshipQuestionnaire(data: any) {
    this.entityDetailsServices.isRelationshipQuestionnaireChanged ? this.leaveCurrentRelationship(data) : this.getQuestionnaire(data);
  }

  leaveCurrentRelationship(data: any) {
    this.emitLeaveModal.emit({ details : data, isLeaveFromRelationTab : true });
  }

  saveOrAddRelationshipModal() {
    if(this.entityDetailsServices.isRelationshipQuestionnaireChanged) {
      this.entityDetailsServices.globalSave$.next();
    }
    document.getElementById('open-relationship-modal').click();
  }

  addRelation() {
    if (!this.isSaving && this.validateRelationship()) {
      this.isSaving = true;
      const REQ_BODY = {
        'questionnaireAnsHeaderId': null,
        'personEntityId': this._activatedRoute.snapshot.queryParamMap.get('personEntityId'),
        'validPersonEntityRelTypeCodes': this.getSelectedRelationTypeCodes().map(typeCode => Number(typeCode))
      };
      this.$subscriptions.push(this.entityDetailsServices.saveOrUpdateCoiFinancialEntityDetails(REQ_BODY).subscribe((res: any) => {
        res.forEach(ele => {
          this.definedRelationships.push(ele);
          this.findRelation(ele.validPersonEntityRelType.relationshipTypeCode);
        });
        this.getQuestionnaire(res[0]);
        this.entityDetailsServices.isShowRelationButton = this.availableRelationships.length;
        this.clearRelationModal();
        this.isSaving = false;
        this.updateRelationship.emit(res);
       }, error => {
        this.isSaving = false;
        if (error.status === 405) {
          document.getElementById('hide-relationship-modal').click();
          this.entityDetailsServices.concurrentUpdateAction = 'Add Relationship';
      } else {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
      }
      }));
    }
  }

  getSelectedRelationTypeCodes() {
    return Object.keys(this.isChecked).filter(key => this.isChecked[key]);
  }

  private findRelation(financialEntityRelTypeCode: string) {
    const RELATION_INDEX = this.availableRelationships.findIndex(element =>
      element.personEntityRelType.relationshipTypeCode === financialEntityRelTypeCode);
    if (RELATION_INDEX !== -1) {
      this.availableRelationships.splice(RELATION_INDEX, 1);
    }
  }

  clearRelationModal() {
    document.getElementById('hide-relationship-modal').click();
    this.coiFinancialEntityDetail.personEntityRelType = null;
    this.isChecked = {};
  }
  private removeExistingRelation() {
    if (this.definedRelationships.length) {
      this.activeRelationship = this.definedRelationships[0].validPersonEntityRelType.relationshipTypeCode;
      this.definedRelationships.forEach(element => {
        this.findRelation(element.validPersonEntityRelType.personEntityRelType.relationshipTypeCode);
      });
    }
    this.entityDetailsServices.isShowRelationButton = this.availableRelationships.length;
  }
  validateRelationship() {
    this.relationValidationMap.clear();
    if (!this.getSelectedRelationTypeCodes().length) {
      this.relationValidationMap.set('relationRadio', 'Please select a relation to continue.');
    }
    return this.relationValidationMap.size === 0 ? true : false;
  }

  clearModal() {
    this.relationValidationMap.clear();
    this.isChecked = {};
  }

  questionnaireSaveAction(event) {
    this.entityDetailsServices.$saveQuestionnaireAction.next(event);
  }

  questionnaireEdit(event) {
    this.entityDetailsServices.isRelationshipQuestionnaireChanged = true;
    let nameOfQuestionnaire = this.definedRelationships.find(ele => ele.validPersonEntityRelType.personEntityRelType.relationshipTypeCode == this.activeRelationship);
    if(!this.entityDetailsServices.unSavedSections.some(ele => ele.includes('Relationship Questionnaire'))) {
      this.entityDetailsServices.unSavedSections.push( nameOfQuestionnaire.validPersonEntityRelType.personEntityRelType.description +' Relationship Questionnaire');
    }
  }

  deleteRelationship() {
    let removeRelId = this.currentRelationshipDetails.personEntityRelId;
    this.$subscriptions.push(this.entityDetailsServices.deletePersonEntityRelationship
      (this.currentRelationshipDetails.personEntityRelId, this.currentRelationshipDetails.personEntityId).subscribe(async (updatedTimestamp) => {
        this.availableRelationships = await this.getRelationshipLookUp();
        await this.getDefinedRelationships();
        this.deleteRelationshipEvent.emit({'updatedTimestamp': updatedTimestamp,'removeRelId': removeRelId, 'isDeleted': true}); 
        if(this.definedRelationships.length) {
          this.getQuestionnaire(this.definedRelationships[0]);
        }
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationship deleted successfully.');
      }, _err => {
        if (_err.status === 405) {
          this.entityDetailsServices.concurrentUpdateAction = 'Delete Relationship'
      } else {
    this._commonService.showToast(HTTP_ERROR_STATUS, `Error in deleting relationship.`);
      }
      }));
  }

  leaveCurrentTab() {
    this.$subscriptions.push(this.entityDetailsServices.$relationshipTabSwitch.subscribe(selectedQuestionnaire => {
      this.getQuestionnaire(selectedQuestionnaire);
    }));
  }
}
