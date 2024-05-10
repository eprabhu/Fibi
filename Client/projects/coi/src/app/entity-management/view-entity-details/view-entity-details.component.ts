import { Component, EventEmitter, HostListener, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityManagementService } from '../../entity-management/entity-management.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { NavigationService } from '../../common/services/navigation.service';
import { environment } from '../../../environments/environment';
import { EntityDetailsService } from '../../disclosure/entity-details/entity-details.service';
import { SfiService } from '../../disclosure/sfi/sfi.service';
import { getEndPointOptionsForEntity } from '../../../../../fibi/src/app/common/services/end-point.config';
import { fadeInOutHeight, heightAnimation } from '../../common/utilities/animations';
import { hideModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';

declare const $: any;
@Component({
    selector: 'app-view-entity-sfi-details',
    templateUrl: './view-entity-details.component.html',
    styleUrls: ['./view-entity-details.component.scss'],
    animations: [fadeInOutHeight, heightAnimation('0', '*', 300, 'heightAnimation')]
})
export class ViewEntityDetailsComponent implements OnInit, OnDestroy {

    @Input() entityDetails: any;
    entityId: any;
    $subscriptions: Subscription[] = [];
    previousURL = '';
    deployMap = environment.deployUrl;
    hasManageEntity = false;
    valueOfModify = '';
    mandatoryList = new Map();
    modifyType = '';
    modifyDescription = '';
    inactivateReason = '';
    reasonValidateMapEntity = new Map();
    revisionReason = '';
    entityRelationshipValue = null;
    clearField: String;
    EntitySearchOptions: any = {};
    approveEntityValidateMap = new Map();
    relationshipEntityName: String = '';
    relationshipEntityId = null;
    entityRelationshipDescription = '';
    relationshipLookUpList: any = [];
    entityRelationshipNumber = null;
    isCardExpanded = true;
    @Output() modifiedEntityId = new EventEmitter<any>();
    @Output() approveEntityDetails = new EventEmitter<any>();
    @Input() isTriggeredFromSlider: boolean = false;
    isShowRiskHistory = false;
    isOpenSlider = false;
    isConcurrency = false;
    isUserCollapse = false;
    readMore: string;

    constructor(private _router: Router, private _route: ActivatedRoute,
        public entityManagementService: EntityManagementService,
        private _commonServices: CommonService,
        private _navigationService: NavigationService,
        public entityDetailsServices: EntityDetailsService, public sfiService: SfiService) {
    }

    ngOnInit() {
        this.hasManageEntity = this._commonServices.rightsArray.includes('MANAGE_ENTITY');
        this.getRelationshipTypes();
    }

    ngOnDestroy() {
        this.sfiService.isShowSfiNavBar = false;
        subscriptionHandler(this.$subscriptions);
    }

    navigateBack() {
        if (this._navigationService.previousURL.includes('entityManageId') || this._navigationService.previousURL === '') {
            this._router.navigate(['/coi/entity-management']);
        } else {
            this._router.navigateByUrl(this._navigationService.previousURL);
        }

    }

    updatedEntityDetails(event) {
        if (event) {
            this.revisionReason = '';
            this.modifyType = '';
            this.entityDetails.entityId = event;
            this.modifiedEntityId.emit({ entityId: event });
            this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: this.entityDetails.entityId }, queryParamsHandling: 'merge' });
        }
    }

    redirectUrl(url) {
        if (url.includes('http')) {
            window.open(url, '_blank');
        } else {
            window.open('//' + url, '_blank');
        }
    }

    openConfirmationModal() {
        this.readMore = 'false';
        this.mandatoryList.clear();
        $('#modifyEntityConfirmationModal').modal('show');
    }

    modifyEntity() {
        this.mandatoryList.clear();
        if (this.validationCheck()) {
            this.readMore = '';
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
            this.mandatoryList.set('change', 'Please choose a modification type.');
        }
        if (!this.modifyDescription) {
            this.mandatoryList.set('description', 'Please provide a reason for modifying the entity.');
        }
        return this.mandatoryList.size === 0 ? true : false;
    }

    closeModal() {
        this.mandatoryList.clear();
        this.valueOfModify = '';
        this.modifyDescription = '';
        this.readMore = '';
        $('#modifyEntityConfirmationModal').modal('hide');
    }

    goToHome() {
        this._router.navigate(['/coi/user-dashboard']);
    }

    activateInactivateEntity() {
        this.readMore = 'false';
        this.reasonValidateMapEntity.clear();
        document.getElementById('inactivate-confirm-message').click();
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
                entityId: this.entityDetails.entityId,
                isActive: !this.entityDetails.isActive,
                revisionReason: this.inactivateReason,
                entityNumber: this.entityDetails.entityNumber
            };
            this.$subscriptions.push(this.entityManagementService.activateInactivate(REQ_BODY).subscribe((res: any) => {
                this.inactivateReason = '';
                this.readMore = '';
                document.getElementById('hide-inactivate-modal').click();
                this._commonServices.showToast(HTTP_SUCCESS_STATUS, `Entity ${this.entityDetails.isActive ? 'inactivated' : 'activated '} successfully`);
                const entityId = Number(this.entityDetails.entityId);
                entityId === res.entityId ? this.modifiedEntityId.emit(res) :
                    this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: res.entityId } });
            }, error => {
                if (error.status === 405) {
                    this.readMore = '';
                    hideModal('inactivateConfirmationModal');
                    this.entityManagementService.concurrentUpdateAction = `${this.entityDetails.isActive ? 'Inactivate' : 'Activate '} Entity`;
                } else {
                    this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                }
            }));
        }
    }

    validForActivateAndInactivateEntity(): boolean {
        if (!this.inactivateReason && this.entityDetails.isActive) {
            this.reasonValidateMapEntity.set('reason', '*Please enter the reason for inactivation.');
        }
        return this.reasonValidateMapEntity.size === 0 ? true : false;
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
            this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonServices.baseUrl, 'ALL');
        }
    }

    approveEntity() {
        this.approveEntityValidateMap.clear();
        if (this.validateApproveEntity()) {
            const REQ_BODY = {
                entityId: this.entityDetails.entityId,
                entityNumber: this.entityDetails.entityNumber,
                entityRelTypeCode: this.entityRelationshipValue,
                nodeId: this.entityRelationshipNumber,
                nodeTypeCode: 1
            };
            this.$subscriptions.push(this.entityManagementService.approveEntity(REQ_BODY).subscribe((res: any) => {
                this.entityDetails.entityStatus.entityStatusCode = res.entityStatusCode;
                this.entityDetails.updatedUserFullName = res.updatedUserFullName;
                this.entityDetails.updateTimestamp = res.updateTimestamp;
                this.approveEntityDetails.emit(this.entityDetails);
                document.getElementById('hide-approve-entity-modal').click();
                this.clearApproveEntityFiled();
                this._commonServices.showToast(HTTP_SUCCESS_STATUS, `Entity approved successfully.`);
            }, error => {
                if (error.status === 405) {
                    hideModal('approve-entity-modal');
                    this.entityManagementService.concurrentUpdateAction = 'Approve Entity';
                } else {
                    this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                }
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
        if (this.entityRelationshipValue && this.entityRelationshipValue !== '1' && !this.relationshipEntityName) {
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
        this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonServices.baseUrl, 'ALL');
        this.entityRelationshipNumber = null;
    }

    toggleSlider() {
            this.$subscriptions.push(this.entityManagementService.riskAlreadyModified({
                'riskCategoryCode': this.entityDetails.entityRiskCategory.riskCategoryCode,
                'entityId': this.entityDetails.entityId
            }).subscribe((data: any) => {
                this.isOpenSlider = !this.isOpenSlider;
            }, err => {
                if (err.status === 405) {
                    this.entityManagementService.concurrentUpdateAction = 'Entity Risk Status';
                } else {
                    this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                }
            }))
    }

    cancelConcurrency() {
        this.entityManagementService.concurrentUpdateAction = '';
    }

    riskSliderClosed(event) {
        this.isOpenSlider = event;
    }

    getSfiEntityDetails() {
        this.$subscriptions.push(this.entityDetailsServices.getCoiEntityDetails(this.entityId).subscribe((res: any) => {
          this.entityDetails = res.coiEntity;
        }, _error => {
          this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
      }

    /* Auto expand and collapse based on screen responsiveness */
    @HostListener('window:resize', ['$event'])
    listenScreenSize() {
        if (!this.isUserCollapse) {
            this.isCardExpanded = !(window.innerWidth <= 992);
        }
    }

    closeActiveInactiveModal() {
        this.inactivateReason='';
        this.readMore = '';
        document.getElementById('hide-inactivate-modal').click();
    }

}
