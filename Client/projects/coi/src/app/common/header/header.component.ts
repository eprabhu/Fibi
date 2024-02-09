import {Component, OnDestroy, OnInit, ViewEncapsulation} from '@angular/core';
import {environment} from '../../../environments/environment';
import {NavigationEnd, Router} from '@angular/router';
import {CommonService} from '../services/common.service';
import {Subscription} from 'rxjs';
import {subscriptionHandler} from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HeaderService } from './header.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';

declare const $: any;
class ChangePassword {
    password = '';
    reEnterPassword = '';
}

@Component({
    selector: 'app-header',
    templateUrl: './header.component.html',
    styleUrls: ['./header.component.scss'],
    encapsulation: ViewEncapsulation.None
})
export class HeaderComponent implements OnInit, OnDestroy {

    logo: any;
    isAccessible = false;
    personId: any;
    fullName = '';
    clearField: String = '';
    loginPerson = this.commonService.getCurrentUserDetail('externalReviewerRight');
    isMaleUser = this.commonService.getCurrentUserDetail('gender') === 'M';
    isAdmin = true;
    resetPassword = new ChangePassword();
    showReEnterPassword = false;
    showPassword = false;
    passwordValidation = new Map();
    timer: any = {password: null, confirmPassword: null};
    $subscriptions: Subscription[] = [];
    homeNavigation: string = '';
    isAdministrator: boolean = false;
    ispersondetailsmodal = false;
    userDetails = null;
    noteComment: any;
    isShowCreateOrReviseModal = false;
    triggeredFrom = '';
    reviseObject: any = { revisionComment: null, disclosureId: null };
    isShowNavBarOverlay = false;

    constructor(public router: Router,
                public commonService: CommonService, public headerService: HeaderService) {
        this.logo = environment.deployUrl + './assets/images/logo.png';
        // document.addEventListener('click', this.offClickSideBarHandler.bind(this));
    }

    // offClickSideBarHandler(event) {
        
    // }

    onClickMenuBar() {
        if (document.getElementById('responsive-nav').classList.contains('show-menu')) {
            document.getElementById('responsive-nav').classList.remove('show-menu');
            this.isShowNavBarOverlay = false;
        } else {
            this.isShowNavBarOverlay = true;
            document.getElementById('responsive-nav').classList.add('show-menu');
        }
    }

    ngOnInit() {
        this.fullName = this.commonService.getCurrentUserDetail('fullName');
        this.isAdministrator = this.commonService.getAvailableRight(['COI_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_COI'])
            || this.commonService.isCoiReviewer;
        this.navigateForHomeIcon();
        this.userDetails = {
            personId: this.commonService.getCurrentUserDetail('personId'),
            fullName: this.commonService.getCurrentUserDetail('fullName')
        };
        this.openModalTriggeredFromChild();
    }

    redirectToOpa() {
        this.router.navigate(['/coi/opa/form'],
            {queryParams: {disclosureId: 2}});
    }

    navigateForHomeIcon(): void {
        this.homeNavigation = this.isAdministrator ? '#/coi/admin-dashboard' : '#/coi/user-dashboard/disclosures';
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    logout() {
        this.router.navigate(['/logout']);
    }

    changePassword() {
        if (this.isValidPassword()) {
            // this.$subscriptions.push(this._commonService
            //     .changeExternalReviewerPassword({newPassword: this.resetPassword.password})
            //     .subscribe((res: any) => {
            //         this.clearPasswordFields();
            //         document.getElementById('reset-pass-btn')?.click();
            //         this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Password changed successfully');
            //     }, (err) => this._commonService.showToast(HTTP_ERROR_STATUS, err.error)));
        }
    }

    clearPasswordFields() {
        this.passwordValidation.clear();
        this.resetPassword = new ChangePassword();
    }

    isValidPassword(): boolean {
        this.passwordValidation.clear();
        this.passwordAtleast7Characters();
        this.confirmPasswordSame();
        return !this.passwordValidation.size;
    }

    passwordLengthValidator() {
        clearTimeout(this.timer.password);
        this.timer.password = setTimeout(() => {
            this.resetPassword.password = this.resetPassword.password.trim();
            this.passwordValidation.delete('password-length');
            this.passwordAtleast7Characters();
        });
    }

    checkSamePassword() {
        this.resetPassword.reEnterPassword = this.resetPassword.reEnterPassword.trim();
        if (this.resetPassword.reEnterPassword) {
            clearTimeout(this.timer.confirmPassword);
            this.timer.confirmPassword = setTimeout(() => {
                this.passwordValidation.delete('same-password');
                this.confirmPasswordSame();
            }, 500);
        }
    }

    triggerClickForId(modalId: string) {
        if (modalId) {
            document.getElementById(modalId).click();
        }
    }

    closePersonDetailsModal(event) {
        this.ispersondetailsmodal = event;
    }

    private passwordAtleast7Characters() {
        if (this.resetPassword.password.length < 7) {
            this.passwordValidation.set('password-length', true);
        }
    }

    private confirmPasswordSame() {
        if (this.resetPassword.password !== this.resetPassword.reEnterPassword) {
            this.passwordValidation.set('same-password', true);
        }
    }

    saveOrUpdateNote() {
        if (this.noteComment.trim()) {
            this.$subscriptions.push(this.headerService.saveOrUpdatePersonNote({
                'personId': this.commonService.getCurrentUserDetail('personId'),
                'content': this.noteComment.trim()
            }).subscribe((ele: any) => {
                this.commonService.isShowCreateNoteModal = false;
                this.noteComment = '';
                if(this.router.url.includes('/coi/user-dashboard/notes')) {
                    this.commonService.$updateLatestNote.next(ele);
                }
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Note added successfully.');
            } , error => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in adding note, please try again.');
            }));
        }
    }

    showNotes() {
        this.commonService.isShowCreateNoteModal = true;
        setTimeout(() => {
            document.getElementById("textArea").focus();
        });
    }

    closeAddNote() {
        this.commonService.isShowCreateNoteModal = false;
        this.noteComment = '';
    }

    closeModal() {
        this.commonService.isOpenAttachmentModal = false;
    }

    outputEventAction(event) {
        if (event.closeModal != null) {
            this.isShowCreateOrReviseModal = event.closeModal;
        }
    }

    openTravelDisclosure(): void {
        this.triggeredFrom = 'TRAVEL_DISCLOSURE';
        this.getActiveDisclosureAndOpenModal();
    }

    getActiveDisclosureAndOpenModal() {
        this.$subscriptions.push(this.headerService.getActiveDisclosure().subscribe((res: any) => {
            this.headerService.activeDisclosures = res.coiDisclosures || [];
            this.headerService.activeOPAs = res.opaDisclosure || [];
            this.isShowCreateOrReviseModal = true;
        }));
    }

    openProjectDisclosure() {
        this.triggeredFrom = 'PROJECT_DISCLOSURE';
        this.getActiveDisclosureAndOpenModal();
    }

    openReviseModal() {
        this.reviseObject = {revisionComment: null, disclosureId: null};
        this.reviseObject.revisionComment = '';
        this.triggeredFrom = 'FCOI_DISCLOSURE';
        this.isShowCreateOrReviseModal = true;
    }

    openCreateSFI() {
        this.router.navigate(['/coi/create-sfi/create'], { queryParams: { type: 'SFI' } });
    }

    openModalTriggeredFromChild() {
        this.$subscriptions.push(this.headerService.$openModal.subscribe((event: string) => {
            if(event == 'FCOI') {
                this.openReviseModal();
            }
        }))
    }

    changeTheme(themename: string) {
        document.querySelector("html").className = '';
        document.querySelector("html").classList.add(themename);
        $('#dissmiss-btn').click();
    }

    
}
