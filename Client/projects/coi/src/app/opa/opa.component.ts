import {Component, OnInit} from '@angular/core';
import {FormBuilderEvent} from '../shared/form-builder-view/form-builder-interface';
import {Subject} from 'rxjs';
import {OpaService} from './services/opa.service';
import {isEmptyObject} from '../../../../fibi/src/app/common/utilities/custom-utilities';
import {DataStoreService} from './services/data-store.service';
import {CommonService} from '../common/services/common.service';
import {environment} from '../../environments/environment';
import {REPORTER_HOME_URL, HTTP_ERROR_STATUS} from '../app-constants';
import {OPA, OpaDisclosure} from './opa-interface';
import {DefaultAssignAdminDetails, PersonProjectOrEntity, coiReviewComment} from '../shared-components/shared-interface';
import {HTTP_SUCCESS_STATUS} from '../../../../fibi/src/app/app-constants';
import {Router} from '@angular/router';
import {Location} from '@angular/common';
import {ModalType} from '../disclosure/coi-interface';

@Component({
    selector: 'app-opa',
    templateUrl: './opa.component.html',
    styleUrls: ['./opa.component.scss']
})
export class OpaComponent implements OnInit {
    isCardExpanded = true;
    formBuilderEvents = new Subject<FormBuilderEvent>();
    opa: OPA = new OPA();
    deployMap = environment.deployUrl;
    isAddAssignModalOpen = false;
    defaultAdminDetails = new DefaultAssignAdminDetails();
    personProjectDetails = new PersonProjectOrEntity();
    helpTexts = [];
    primaryBtnName = '';
    descriptionErrorMsg = '';
    textAreaLabelName = '';
    withdrawErrorMsg = 'Describe the reason for withdrawing the disclosure';
    returnErrorMsg = 'Describe the reason for returning the disclosure';
    withdrawHelpTexts = [
        `Withdraw any disclosure in 'Submitted' status.`,
        `Describe the reason for withdrawal in the field provided.`,
        `Click on 'Withdraw' button to recall your disclosure for any modification.`
    ];
    returnHelpTexts = [
        `Return any disclosure in 'Review in progress' status.`,
        `Describe the reason for returning  in the field provided.`,
        `Click on 'Return' button to return the disclosure for any modification.`
    ];
    description: any;
    showSlider = false;
    selectedType: string;
    showPersonDetailsModal = false;
    personDetailsModalVO = {personId: '', fullName: ''};
    $subscriptions = [];

    constructor(public opaService: OpaService,
                private _router: Router,
                public location: Location,
                public commonService: CommonService,
                private dataStore: DataStoreService) {
    }

    ngOnInit(): void {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    triggerSave() {
        this.opaService.formBuilderEvents.next({eventType: 'SAVE'});
    }

    submitOPA() {
        this.$subscriptions.push(this.opaService.submitOPA(this.opa.opaDisclosure.opaDisclosureId, this.opa.opaDisclosure.opaDisclosureNumber)
            .subscribe((res: any) => {
                this.opa.opaDisclosure = res;
                this.dataStore.updateStore(['opaDisclosure'], {opaDisclosure: this.opa.opaDisclosure});
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `OPA submitted successfully.`);
            }, err => this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')));
    }

    openAddAssignModal(): void {
        this.isAddAssignModalOpen = true;
        this.setAssignAdminModalDetails();
    }

    closeAssignAdministratorModal(event) {
        if (event && (event.adminPersonId || event.adminGroupId)) {
            // this.getCoiReview();
            this.opa.opaDisclosure = event;
            this.dataStore.updateStore(['opaDisclosure'], this.opa);
        }
        this.isAddAssignModalOpen = false;
    }

    openConfirmationModal(actionBtnName: string, helpTexts: string [] = [], descriptionErrorMsg: string = ''): void {
        this.helpTexts = helpTexts;
        this.primaryBtnName = actionBtnName;
        this.descriptionErrorMsg = descriptionErrorMsg;
        this.textAreaLabelName = actionBtnName === 'Withdraw' ? ' Withdrawal' : 'Return';
        this.setPersonProjectDetails();
        document.getElementById('disclosure-confirmation-modal-trigger-btn').click();
    }

    performDisclosureAction(event): void {
        this.description = event;
        switch (this.primaryBtnName) {
            case 'Return':
                return this.returnDisclosure(event);
            case 'Withdraw':
                return this.withdrawDisclosure(event);
            default:
                return;
        }
    }

    returnDisclosure(event) {
        this.$subscriptions.push(this.opaService
            .returnOPA(this.getRequestObj(event))
            .subscribe((res: any) => {
                this.opa.opaDisclosure = res;
                this.dataStore.updateStore(['opaDisclosure'], this.opa);
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `OPA returned successfully.`);
                this.goToHomeUrl();
            }, _err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, `Error in returning OPA.`);
            }));
    }

    goToHomeUrl() {
        // TODO admin/reviewer/pi based redirect once rights are implemented.
        const reRouteUrl = this.opaService.previousHomeUrl || REPORTER_HOME_URL;
        this._router.navigate([reRouteUrl]);
    }

    withdrawDisclosure(event) {
        this.$subscriptions.push(this.opaService
            .withdrawOPA(this.getRequestObj(event))
            .subscribe((res: any) => {
                this.opa.opaDisclosure = res;
                this.dataStore.updateStore(['opaDisclosure'], this.opa);
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `OPA withdrawn successfully.`);
            }, _err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, `Error in withdrawing disclosure.`);
            }));
    }

    getRequestObj(description) {
        return {
            'opaDisclosureId' : this.opa.opaDisclosure.opaDisclosureId,
            'opaDisclosureNumber' : this.opa.opaDisclosure.opaDisclosureNumber,
            'comment': description
        }
    }

    closePersonDetailsModal(event) {
        this.showPersonDetailsModal = event;
    }

    openDetailModal(): void {
        this.personDetailsModalVO.personId = this.opa.opaDisclosure.personId;
        this.personDetailsModalVO.fullName = this.opa.opaDisclosure.personName;
        this.showPersonDetailsModal = true;
    }

    private getDataFromStore() {
        this.opa = this.dataStore.getData();
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private setAssignAdminModalDetails(): void {
        this.defaultAdminDetails.adminGroupId = this.opa.opaDisclosure.adminGroupId;
        this.defaultAdminDetails.adminGroupName = this.opa.opaDisclosure.adminGroupName;
        this.defaultAdminDetails.adminPersonId = this.opa.opaDisclosure.adminPersonId;
        this.defaultAdminDetails.adminPersonName = this.opa.opaDisclosure.adminPersonName;
    }

    private setPersonProjectDetails(): void {
        this.personProjectDetails.personFullName = this.opa.opaDisclosure.opaPerson.personName;
        // this.personProjectDetails.projectDetails = this.coiData?.projectDetail;
        this.personProjectDetails.unitDetails = this.opa.opaDisclosure.homeUnitName;
    }

    completeDisclosureReview() {
        this.$subscriptions.push(this.opaService
            .completeOPAReview(this.opa.opaDisclosure.opaDisclosureId, this.opa.opaDisclosure.opaDisclosureNumber)
            .subscribe((res: any) => {
                this.opa.opaDisclosure.reviewStatusType = res.reviewStatusType;
                this.opa.opaDisclosure.dispositionStatusType = res.dispositionStatusType;
                this.opa.opaDisclosure.reviewStatusCode = res.reviewStatusCode;
                this.dataStore.updateStore(['opaDisclosure'], {opaDisclosure: this.opa.opaDisclosure});
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `Review completed successfully.`);
            }, _err => {
                // if (_err.error.text === 'REVIEW_STATUS_NOT_COMPLETE') {
                //     document.getElementById('reviewPendingCompleteReviewErrorModalTrigger').click();
                // } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in completing review.`);
                // }
            }));
    }

    updateOpaReview(modalType: ModalType) {
        const reviewerInfo = this.opa.opaReviewerList.find(ele =>
            ele.assigneePersonId === this.commonService.currentUserDetails.personId);
        if (reviewerInfo) {
            this.opaService.$SelectedReviewerDetails.next(reviewerInfo);
            this.opaService.triggerStartOrCompleteCoiReview(modalType);
            this.opaService.isEnableReviewActionModal = true;
        }
    }

    checkForOPAAdmin() {
        return this.commonService.getAvailableRight(['MANAGE_OPA_DISCLOSURE']);
    }

    isLoggedInUser(personId: string) {
        return this.commonService?.getCurrentUserDetail('personId') === personId;
    }

    openSlider(type, count) {
        if(count) {
            this.showSlider = true;
            this.selectedType = type;
        }
    }

    closeHeaderSlider() {
        this.showSlider = false;
        this.selectedType = '';
    }

    openReviewComment() {
        const COMMENT_META_DATA: coiReviewComment = {
            documentOwnerPersonId: this.opa.opaDisclosure.personId,
            componentTypeCode: '9',
            subModuleItemKey: null,
            subModuleItemNumber : null
        }
        this.commonService.$commentConfigurationDetails.next(COMMENT_META_DATA);
        this.opaService.isShowCommentNavBar = true;
    }

    closeReviewComment(event) {
        this.opaService.isShowCommentNavBar = event;
    }

}
