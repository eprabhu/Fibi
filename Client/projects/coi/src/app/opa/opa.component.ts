import { Component, HostListener, OnInit} from '@angular/core';
import { FormBuilderEvent} from '../configuration/form-builder-create/shared/form-builder-view/form-builder-interface';
import { Subject} from 'rxjs';
import { OpaService} from './services/opa.service';
import { hideModal, openModal} from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { DataStoreService} from './services/data-store.service';
import { CommonService} from '../common/services/common.service';
import { environment} from '../../environments/environment';
import { REPORTER_HOME_URL, HTTP_ERROR_STATUS} from '../app-constants';
import { OPA } from './opa-interface';
import { DefaultAssignAdminDetails, PersonProjectOrEntity, coiReviewComment} from '../shared-components/shared-interface';
import { HTTP_SUCCESS_STATUS} from '../../../../fibi/src/app/app-constants';
import { Router} from '@angular/router';
import { Location} from '@angular/common';
import { ModalType} from '../disclosure/coi-interface';
import { NavigationService} from "../common/services/navigation.service";
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

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
    helpTexts = '';
    primaryBtnName = '';
    descriptionErrorMsg = '';
    textAreaLabelName = '';
    withdrawErrorMsg = 'Please provide the reason for withdrawing the disclosure.';
    returnErrorMsg = 'Please provide the reason for returning the disclosure.';
    completeReviewHelpText = 'You are about to complete the disclosure\'s final review.'
    confirmationHelpTexts = '';
    returnHelpTexts = 'Please provide the reason for return.';
    withdrawHelpTexts = 'Please provide the reason for withdrawal.';
    submitHelpTexts = 'You are about to submit the OPA disclosure.';
    returnModalHelpText = 'You are about to return the OPA disclosure.';
    withdrawModalHelpText = 'You are about to withdraw the OPA disclosure.';
    description: any;
    showSlider = false;
    selectedType: string;
    $subscriptions = [];
    commentsRight: {
        canViewPrivateComments: boolean;
        canMaintainPrivateComments: boolean;
    };
    isHomeClicked = false;
    isSubmitClicked = false;
    isUserCollapse = false;
    validationList = [];

    constructor(public opaService: OpaService,
                private _router: Router,
                public location: Location,
                public commonService: CommonService,
                private _navigationService: NavigationService,
                public dataStore: DataStoreService) {
    }

    ngOnInit(): void {
        this.getDataFromStore();
        this.setPersonProjectDetails();
        this.listenDataChangeFromStore();
        this.subscribeSaveComplete();
        this.setTopDynamically();
        // this.commentsRight.canViewPrivateComments = this.commonService.getAvailableRight(['VIEW_OPA_PRIVATE_COMMENTS']);
        // this.commentsRight.canMaintainPrivateComments = this.commonService.getAvailableRight(['MAINTAIN_OPA_PRIVATE_COMMENTS']);
    }

    triggerSave() {
        this.isSubmitClicked = false;
        this.opaService.formBuilderEvents.next({eventType: 'SAVE'});
    }

    opaSubmissionModal() {
        this.isSubmitClicked = true;
        if (this.opaService.isFormBuilderDataChangePresent) {
            this.opaService.formBuilderEvents.next({ eventType: 'SAVE' });
        } else {
            this.validationList = [];
            this.validateForm();
        }
    }

    saveAndSubmit() {
        this.submitOPA();
    }

    subscribeSaveComplete() {
        this.$subscriptions.push(this.opaService.triggerSaveComplete.subscribe((data: any) => {
            if(data && (this.validationList?.length || this.isSubmitClicked)) {
                this.validateForm();
            }
        }))
    }

    submitOPA() {
        this.$subscriptions.push(this.opaService.submitOPA(this.opa.opaDisclosure.opaDisclosureId, this.opa.opaDisclosure.opaDisclosureNumber)
            .subscribe((res: any) => {
                this.opa.opaDisclosure = res;
                this.isSubmitClicked = false;
                this.dataStore.updateStore(['opaDisclosure'], {opaDisclosure: this.opa.opaDisclosure});
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `OPA submitted successfully.`);
            }, err => {
                this.isSubmitClicked = false;
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')
            }));
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

    openConfirmationModal(actionBtnName: string, helpTexts: string = '', descriptionErrorMsg: string = '', modalHelpText: string = ''): void {
        this.primaryBtnName = actionBtnName;
        this.descriptionErrorMsg = descriptionErrorMsg;
        this.textAreaLabelName = actionBtnName === 'Withdraw' ? ' Withdrawal' : 'Return';
        this.setPersonProjectDetails();
        this.confirmationHelpTexts = '';
        this.helpTexts = '';
        setTimeout(() => {
            this.confirmationHelpTexts = modalHelpText;
            this.helpTexts = helpTexts;
            document.getElementById('disclosure-confirmation-modal-trigger-btn').click();
        });
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
        this.isHomeClicked = true;
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
            }, err => {
                if (err.status === 405) {
                    hideModal('disclosure-confirmation-modal');
                    this.opaService.concurrentUpdateAction = 'Withdraw Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in withdrawing disclosure.`);
                }
            }));
    }

    getRequestObj(description) {
        return {
            'opaDisclosureId' : this.opa.opaDisclosure.opaDisclosureId,
            'opaDisclosureNumber' : this.opa.opaDisclosure.opaDisclosureNumber,
            'comment': description
        }
    }

    openPersonDetailsModal(): void {
        this.commonService.openPersonDetailsModal(this.opa.opaDisclosure.personId)
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
        this.personProjectDetails.unitNumber = this.opa.opaDisclosure.homeUnit;
        this.personProjectDetails.unitName = this.opa.opaDisclosure.homeUnitName;
        this.personProjectDetails.homeUnit = this.opa.opaDisclosure.homeUnit;
        this.personProjectDetails.homeUnitName = this.opa.opaDisclosure.homeUnitName;
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
            ele.assigneePersonId === this.commonService.currentUserDetails.personId && ele.reviewStatusTypeCode != '3');
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

    cancelConcurrency() {
        this.opaService.concurrentUpdateAction = '';
    }

    leavePageClicked() {
        this.opaService.isFormBuilderDataChangePresent = false;
        if (this.isHomeClicked) {
           this.goToHomeUrl();
        } else {
            this._router.navigateByUrl(this._navigationService.navigationGuardUrl);
        }
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    collapseHeader() {
        this.isCardExpanded=!this.isCardExpanded;
        this.isUserCollapse=!this.isUserCollapse;
        this.setTopDynamically();
    }

    setTopDynamically() {
        setTimeout(() => {
            const STICKY_HEADER = document.querySelector<HTMLElement>('.header-sticky');
            if(STICKY_HEADER) {
                const elements = document.querySelectorAll('.form-builder-sticky-header');
                if(elements.length) {
                    elements.forEach((element: HTMLElement) => {
                        element.style.top = STICKY_HEADER.offsetHeight + 50 + 'px';
                    });
                }
                const TABLE_STICKY_HEADER = document.querySelectorAll('.form-builder-table-header');
                if(TABLE_STICKY_HEADER.length) {
                    TABLE_STICKY_HEADER.forEach((element: HTMLElement) => {
                        element.style.top = STICKY_HEADER.offsetHeight + 101 + 'px';
                    });
                }
            }
        }, 1000)
    }

    @HostListener('window:resize', ['$event'])
    listenScreenSize() {
        if(!this.isUserCollapse) {
            this.isCardExpanded = window.innerWidth > 1399;
        }
        this.setTopDynamically();
    }

    private validateForm(): void {
        this.opaService.validateForm({
            formBuilderIds: this.opa.opaDisclosure.opaFormBuilderDetails.map(e => e.formBuilderId),
            moduleItemCode: '23',
            moduleSubItemCode: '0',
            moduleItemKey: this.opa.opaDisclosure.opaDisclosureId.toString(),
            moduleSubItemKey: '0',
        }).subscribe((data: any) => {
            this.validationList = data;
            if (!this.validationList.length && this.isSubmitClicked) {
                openModal('opa-submit-confirm-modal');
            }
        }, err => {
            this.commonService.showToast(HTTP_ERROR_STATUS, `Error occurred during from validation.`);
        });
    }

}
