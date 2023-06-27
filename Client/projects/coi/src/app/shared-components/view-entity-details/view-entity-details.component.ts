import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output, SimpleChanges } from '@angular/core';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { slowSlideInOut } from '../../../../../fibi/src/app/common/utilities/animations';
import { EntityManagementService } from '../../entity-management/entity-management.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { NavigationService } from '../../common/services/navigation.service';
import { environment } from '../../../environments/environment';
import { EntityDetailsService } from '../../disclosure/entity-details/entity-details.service';
import { SfiService } from '../../disclosure/sfi/sfi.service';

declare var $: any;
@Component({
  selector: 'app-view-entity-sfi-details',
  templateUrl: './view-entity-details.component.html',
  styleUrls: ['./view-entity-details.component.scss']
})
export class ViewEntityDetailsComponent implements OnInit, OnDestroy,OnChanges {

  entityDetails: any = {};
  entityId: any;
  $subscriptions: Subscription[] = [];
  previousURL = '';
  deployMap = environment.deployUrl;
  isEntityManagement = false;
  isModifyEntity = false;
  @Input() entityIdFromSlider: boolean = false;
  @Input() isTriggeredFromSlider: boolean = false;
  @Output() emitRelationshipModal: EventEmitter<boolean> = new EventEmitter<boolean>();
  valueOfModify = '';
  mandatoryList = new Map();
  changeType = '';
  modifyDescription = '';
  inactivateReason = '';
  reasonValidateMapEntity = new Map();
  isEnableActivateInactivateSfiModal = false;
  @Input() sfiRelationStatus: any = {};
  isRelationshipActive = false;
  sfiStatus = '';
  canMangeSfi = false;
  @Input() isEditMode = false;

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
  }

  getEntityID() {
    this.$subscriptions.push(this._route.queryParams.subscribe(params => {
      this.entityId = this.isEntityManagement ? params.entityManageId : params.personEntityId;
      this.entityId = this.entityId ? this.entityId : this.entityIdFromSlider;
      this.getEntityDetails();
    }));
  }

  ngOnChanges() {
    if (!this.isEntityManagement) {
      this.sfiStatus =  this.getSfiStatus();
      this.canMangeSfi = this.sfiRelationStatus.personId === this._commonServices.currentUserDetails.personId ? true : false;
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
    this.emitRelationshipModal.emit(true);
  }

  saveRelationship() {
    this.entityDetailsServices.globalSave$.next();
  }

  setDesignDiv(): string {
    return ((this.isEntityManagement && this.isModifyEntity) || !this.isEntityManagement) ? 'd-flex justify-content-around' : '';
  }

  openConfirmationModal() {
      $('#modifyEntityConfirmationModal').modal('show');
  }

  modifyEntity() {
    this.mandatoryList.clear();
    if (this.validationCheck()) {
      $('#modifyEntityConfirmationModal').modal('hide');
      this.sfiService.isShowSfiNavBar = true;
      this.changeType = this.valueOfModify;
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
        active: !this.entityDetails.isActive,
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
}
