import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityManagementService } from '../../entity-management/entity-management.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription, forkJoin } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { NavigationService } from '../../common/services/navigation.service';
import { environment } from '../../../environments/environment';
import { EntityDetailsService } from '../../disclosure/entity-details/entity-details.service';
import { SfiService } from '../../disclosure/sfi/sfi.service';
import { getEndPointOptionsForEntity } from '../../../../../fibi/src/app/common/services/end-point.config';
import { fadeInOutHeight } from '../../../../../fibi/src/app/common/utilities/animations';

declare const $: any;
@Component({
  selector: 'app-view-entity-sfi-details',
  templateUrl: './view-entity-details.component.html',
  styleUrls: ['./view-entity-details.component.scss'],
  animations: [fadeInOutHeight]
})
export class ViewEntityDetailsComponent implements OnInit, OnDestroy, OnChanges {

  entityDetails: any = {};
  entityId: any;
  $subscriptions: Subscription[] = [];
  previousURL = '';
  deployMap = environment.deployUrl;
  isEntityManagement = false;
  isModifyEntity = false;
  @Input() isEditMode = false;
  @Input() sfiRelationStatus: any = {};
  @Input() personEntityRelationships = [];
  @Input() personEntity = {};
  @Input() entityIdFromSlider: boolean = false;
  @Input() isTriggeredFromSlider: boolean = false;
  @Output() emitRelationshipModal: EventEmitter<boolean> = new EventEmitter<boolean>();
  valueOfModify = '';
  mandatoryList = new Map();
  modifyType = '';
  modifyDescription = '';
  inactivateReason = '';
  reasonValidateMapEntity = new Map();
  isEnableActivateInactivateSfiModal = false;
  isRelationshipActive = false;
  sfiStatus = '';
  canMangeSfi = false;
  revisionReason = '';
  entityRelationshipValue = null;
  clearField: String;
  EntitySearchOptions: any = {};
  approveEntityValidateMap = new Map();
  relationshipEntityName: String = '';
  relationshipEntityId = null;
  entityRelationshipDescription = '';
  isQuestionnaireCompleted = false;
  allRelationQuestionnaires = [];
  relationshipLookUpList: any = [];
  entityRelationshipNumber = null;
  
  constructor(private _router: Router, private _route: ActivatedRoute,
    public entityManagementService: EntityManagementService,
    private _commonServices: CommonService,
    private _navigationService: NavigationService,
    public entityDetailsServices: EntityDetailsService, public sfiService: SfiService) {
  }

  ngOnInit() {
    this.isEntityManagement = this._router.url.includes('entity-management');
    this.isModifyEntity = this._commonServices.rightsArray.includes('MANAGE_ENTITY');
    this.getEntityID();
    this.listenForQuestionnaireSave();
    this.getRelationshipTypes();
  }

  getEntityID() {
    this.$subscriptions.push(this._route.queryParams.subscribe(params => {
      this.entityId = this.isEntityManagement ? params.entityManageId : params.personEntityId;
      this.entityId = this.entityId ? this.entityId : this.entityIdFromSlider;
      this.getEntityDetails();
    }));
  }

  listenForQuestionnaireSave() { 
    this.$subscriptions.push(this.entityDetailsServices.$saveQuestionnaireAction.subscribe((params: any) => {
      if (params) {
        this.getQuestionnaire();
        this.entityDetailsServices.isRelationshipQuestionnaireChanged = false;
        this._commonServices.showToast(HTTP_SUCCESS_STATUS, `Relationship saved successfully `);
      } else {
        this._commonServices.showToast(HTTP_ERROR_STATUS, `Error in saving relationship`);
      }
    }));
  }

  ngOnChanges() {
    if (!this.isEntityManagement) {
      this.sfiStatus = this.getSfiStatus();
      this.canMangeSfi = this.sfiRelationStatus.personId === this._commonServices.currentUserDetails.personId ? true : false;
      if (this.isEditMode && this.sfiStatus == 'Draft') {
        this.getQuestionnaire();
      }
    }
  }

  ngOnDestroy() {
    this.sfiService.isShowSfiNavBar = false;
    subscriptionHandler(this.$subscriptions);
  }

  navigateBack() {
    if (this.isEntityManagement) {
      if (this._navigationService.previousURL.includes('entityManageId') ||
        this._navigationService.previousURL.includes('entity-details') || this._navigationService.previousURL === '') {
        this._router.navigate(['/coi/entity-management']);
      } else {
        this._router.navigateByUrl(this._navigationService.previousURL);
      }
    } else {
      if (this._navigationService.previousURL.includes('personEntityId') ||
        this._navigationService.previousURL.includes('create-sfi/create') || this._navigationService.previousURL === '') {
         this._router.navigate(['/coi/user-dashboard/entities']);
      } else {
        this._router.navigateByUrl(this._navigationService.previousURL);
      }
    }
  }

  getEntityDetails() {
    if (this.isEntityManagement) {
      this.viewEntityDetails();
    } else {
      this.getSfiEntityDetails();
    }
  }

  viewEntityDetails() {
    this.$subscriptions.push(this.entityManagementService.getEntityDetails(this.entityId).subscribe((res: any) => {
      this.entityDetails = res.coiEntity;
    }, _error => {
      this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }));
  }

  getSfiEntityDetails() {
    this.$subscriptions.push(this.entityDetailsServices.getCoiEntityDetails(this.entityId).subscribe((res: any) => {
      this.entityDetails = res.coiEntity;
    }, _error => {
      this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }));
  }

  updatedEntityDetails(event) {
    if (event) {
      this.revisionReason = '';
      this.modifyType = '';
      this.entityId = event;
      this.getEntityDetails();
      this._router.navigate(['/coi/entity-management/entity-details'],{ queryParams: { entityManageId: this.entityId }, queryParamsHandling: 'merge'});
    }
  }

  redirectUrl(url) {
    if (url.includes('http')) {
      window.open(url, '_blank');
    } else {
      window.open('//' + url, '_blank');
    }
  }

  addNewRelationship() {
    if(this.entityDetailsServices.isRelationshipQuestionnaireChanged) {
      this.entityDetailsServices.globalSave$.next();
    }
      this.emitRelationshipModal.emit(true);
  }

  saveRelationship() {
    this.entityDetailsServices.globalSave$.next();
  }

  openConfirmationModal() {
      $('#modifyEntityConfirmationModal').modal('show');
  }

  modifyEntity() {
    this.mandatoryList.clear();
    if (this.validationCheck()) {
      $('#modifyEntityConfirmationModal').modal('hide');
      this.sfiService.isShowSfiNavBar = true;
      this.modifyType = this.valueOfModify;
      this.revisionReason = this.modifyDescription;
      if (this.sfiService.isShowSfiNavBar) {
        this.valueOfModify = '';
        this.modifyDescription = '';
      }
    }
  }

  validationCheck(): boolean {
    if (!this.valueOfModify) {
      this.mandatoryList.set('change', '*Please choose an action');
    }
    if (!this.modifyDescription) {
      this.mandatoryList.set('description', 'Please enter the description for why you want to modify the entity.');
    }
    return this.mandatoryList.size === 0 ? true : false;
  }

  closeModal() {
   this.mandatoryList.clear();
   this.valueOfModify = '';
   this.modifyDescription = '';
   $('#modifyEntityConfirmationModal').modal('hide');
  }

  goToHome() {
    this.isEntityManagement ? this._router.navigate(['/coi/user-dashboard'])
    : this._router.navigate(['/coi/user-dashboard/entities']);
  }

  activateInactivateEntityOrSfi(moduleName: string) {
    if(moduleName === 'Finalize' && this.entityDetailsServices.isRelationshipQuestionnaireChanged) {
      this.entityDetailsServices.globalSave$.next();
    }
    if (moduleName === 'ENTITY') {
      document.getElementById('inactivate-confirm-message').click();
    } else {
      this.isEnableActivateInactivateSfiModal = true;
    }
  }

  entityRiskLevel(description): string {
    switch (description) {
      case 'High': return 'invalid';
      case 'Medium': return 'medium-risk';
      case 'Low': return 'low-risk';
      default: return 'low-risk';
    }
  }

  activateAndInactivateEntity() {
    this.reasonValidateMapEntity.clear();
    if (this.validForActivateAndInactivateEntity()) {
      const REQ_BODY = {
        entityId: this.entityId,
        isActive: !this.entityDetails.isActive,
        revisionReason: this.inactivateReason
      };
      this.$subscriptions.push(this.entityManagementService.activateInactivate(REQ_BODY).subscribe((res: any) => {
        this.inactivateReason = '';
        document.getElementById('hide-inactivate-modal').click();
        this._commonServices.showToast(HTTP_SUCCESS_STATUS, `Entity ${this.entityDetails.isActive ? 'inactivate' : 'activate '} successfully completed `);
        const entityId = Number(this.entityId);
        entityId === res.entityId ? this.updateEntityDetails(res) : 
          this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: res.entityId } });
      }, error => {
        this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
      }));
    }
  }

  validForActivateAndInactivateEntity(): boolean {
    if (!this.inactivateReason && this.entityDetails.isActive) {
      this.reasonValidateMapEntity.set('reason', '*Please enter the reason for inactivation.');
    }
    return this.reasonValidateMapEntity.size === 0 ? true : false;
  }

  updateEntityDetails(data) {
    this.entityId = data.entityId;
    this.viewEntityDetails();
  }

  closeActivateInactivateSfiModal(event) {
    if (event) {
      this.sfiRelationStatus.isRelationshipActive = event.isRelationshipActive;
      if (event.versionStatus) {
        this.sfiRelationStatus.versionStatus = event.versionStatus;
        if (this.isQuestionnaireCompleted) {
          this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: event.personEntityId, mode: 'view' } });
          this.isQuestionnaireCompleted = false;
        }
      }
      this.sfiStatus = this.getSfiStatus();
      this.isEnableActivateInactivateSfiModal = false;
      if (this.entityId !== event.personEntityId) {
        this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: event.personEntityId, mode: 'view' } });
      }
    } else {
      this.isEnableActivateInactivateSfiModal = false;
    }
  }

  getSfiStatus(): string {
    if (this.sfiRelationStatus.isRelationshipActive && this.sfiRelationStatus.versionStatus === 'ACTIVE') {
      return 'Active';
    } else if (!this.sfiRelationStatus.isRelationshipActive && this.sfiRelationStatus.versionStatus === 'ACTIVE') {
      return 'Inactive';
    } else if (this.sfiRelationStatus.versionStatus === 'PENDING') {
      return 'Draft';
    }
  }

  getRelationshipTypes(): void {
    this.$subscriptions.push(this.entityManagementService.getRelationshipTypes().subscribe((res: any) => {
      this.relationshipLookUpList = res;
    }));
  }

  selectEntityRelationship() { 
    if (this.entityRelationshipValue) {
      this.entityRelationshipDescription = this.relationshipLookUpList.find(ele =>
        this.entityRelationshipValue == ele.entityRelTypeCode).description;
    }
    if (this.entityRelationshipValue !== '1') {
      this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonServices.baseUrl);
    }
  }

  approveEntity() {
    this.approveEntityValidateMap.clear();
    if (this.validateApproveEntity()) {
      const REQ_BODY = {
        entityId: this.entityId,
        entityNumber: this.entityDetails.entityNumber,
        entityRelTypeCode: this.entityRelationshipValue,
        nodeId: this.entityRelationshipNumber,
        nodeTypeCode: 1
      };
      this.$subscriptions.push(this.entityManagementService.approveEntity(REQ_BODY).subscribe((res: any) => {
        this.entityDetails.entityStatus.entityStatusCode = res.entityStatusCode;
        this.entityDetails.updateUserFullName = res.updatedUserFullName;
        this.entityDetails.updateTimestamp = res.updateTimestamp;
        document.getElementById('hide-approve-entity-modal')?.click();
        this.clearApproveEntityFiled();
        this._commonServices.showToast(HTTP_SUCCESS_STATUS, `Entity verified successfully.`);
      }));
    }
  }

  selectedEvent(event) {
    this.relationshipEntityName = event ? event.entityName : '';
    this.relationshipEntityId = event ? event.entityId : null;
    this.entityRelationshipNumber = event ? event.entityNumber : null;
  }

  validateApproveEntity(): boolean {
    if (!this.entityRelationshipValue) {
      this.approveEntityValidateMap.set('relationship', '*Please select entity relationship.');
    }
    if (this.entityRelationshipValue && this.entityRelationshipValue !== '1'  && !this.relationshipEntityName) {
      this.approveEntityValidateMap.set('entityName', '*Please choose an entity name.');
    }
    return this.approveEntityValidateMap.size === 0 ? true : false;
  }

  clearApproveEntityFiled() {
    this.entityRelationshipValue = null;
    this.entityRelationshipDescription = '';
    this.clearField = new String('true');
    this.relationshipEntityId = null;
    this.relationshipEntityName = '';
    this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonServices.baseUrl);
    this.entityRelationshipNumber = null;
  }

  getQuestionnaire() {
    const QUEST_REQ_OBJ_LIST = [];
    this.personEntityRelationships.forEach(rel => {
      this.setQuestionnaireRequestObject(rel.validPersonEntityRelTypeCode, QUEST_REQ_OBJ_LIST);
    });
    this.checkQuestionnaireCompleted(QUEST_REQ_OBJ_LIST);
  }

  setQuestionnaireRequestObject(subItemCode, list) {
    list.push(this.getApplicableQuestionnaire({
      moduleItemCode: 8,
      moduleSubItemCode: 801,
      moduleSubItemKey: subItemCode,
      moduleItemKey: this.entityId,
      actionUserId: this._commonServices.getCurrentUserDetail('personId'),
      actionPersonName: this._commonServices.getCurrentUserDetail('fullName'),
      questionnaireNumbers: [],
      questionnaireMode: 'ACTIVE_ANSWERED_UNANSWERED'
    }));
  }

  getApplicableQuestionnaire(requestObject) {
    requestObject = JSON.parse(JSON.stringify(requestObject));
    return this.entityManagementService.getApplicableQuestionnaire(requestObject);
  }

  checkQuestionnaireCompleted(questionList) {
    this.$subscriptions.push(forkJoin(...questionList).subscribe(data => {
      this.allRelationQuestionnaires = [];
      data.forEach((d: any) => this.combineQuestionnaireList(d.applicableQuestionnaire));
      this.isQuestionnaireCompleted = this.isAllQuestionnaireCompleted(this.allRelationQuestionnaires);
    }, err => {
      this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }));
  }

  combineQuestionnaireList(newList) {
    this.allRelationQuestionnaires = [...this.allRelationQuestionnaires, ...newList];
  }

  isAllQuestionnaireCompleted(questionnaireList) {
    return questionnaireList.every(questionnaire => questionnaire.QUESTIONNAIRE_COMPLETED_FLAG === 'Y');
  }

}
