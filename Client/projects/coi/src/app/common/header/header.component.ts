import {Component, HostListener, OnDestroy, OnInit, ViewEncapsulation} from '@angular/core';
import {environment} from '../../../environments/environment';
import {Router} from '@angular/router';
import {CommonService} from '../services/common.service';
import {Subscription} from 'rxjs';
import {subscriptionHandler} from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HeaderService } from './header.service';
import { CONSULTING_REDIRECT_URL, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS, OPA_REDIRECT_URL } from '../../app-constants';

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
    noteComment: any;
    isShowCreateOrReviseModal = false;
    triggeredFrom = '';
    reviseObject: any = { revisionComment: null, disclosureId: null };
    isShowNavBarOverlay = false;
    notesHelpTexts = `You can view and edit notes under the 'My Notes' tab.`;

    constructor(public router: Router,
                public commonService: CommonService, public headerService: HeaderService) {
        this.logo = environment.deployUrl + './assets/images/logo.png';
        // document.addEventListener('click', this.offClickSideBarHandler.bind(this));
    }

    // offClickSideBarHandler(event) {

    // }

    onClickMenuBar() {
        const NAV_ELEMENT = document.getElementById('responsive-nav');
        const IS_MENU_SHOW = NAV_ELEMENT.classList.contains('show-menu');
        const IS_SCREEN = window.innerWidth <= 1300;

        if (IS_MENU_SHOW) {
            NAV_ELEMENT.classList.remove('show-menu');
            if (IS_SCREEN) {
                this.isShowNavBarOverlay = false;
            }
        } else {
            if (IS_SCREEN) {
                this.isShowNavBarOverlay = true;
            }
            NAV_ELEMENT.classList.toggle('show-menu', IS_SCREEN);
        }
    }

    ngOnInit() {
        this.fullName = this.commonService.getCurrentUserDetail('fullName');
        this.isAdministrator = this.commonService.getAvailableRight(['COI_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_COI'])
            || this.commonService.isCoiReviewer;
        this.navigateForHomeIcon();
        this.openModalTriggeredFromChild();
    }

    navigateForHomeIcon(): void {
        this.homeNavigation = this.isAdministrator ? '#/coi/admin-dashboard' : '#/coi/user-dashboard/disclosures';
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    logout() {
        this.$subscriptions.push(this.commonService.signOut().subscribe(
            null,
            null,
            () => {
                this.clearCurrentUserAndGotoLogin();
        }));
    }

    clearCurrentUserAndGotoLogin() {
        if (!this.commonService.enableSSO) {
            ['authKey', 'cookie', 'sessionId', 'currentTab'].forEach((item) => localStorage.removeItem(item));
            this.commonService.currentUserDetails = {};
          }
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
                'personId': this.commonService.getCurrentUserDetail('personID'),
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
        this.isShowCreateOrReviseModal = true;
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
            if (event == 'FCOI') {
                this.openReviseModal();
            }
        }));
    }

    createOPA() {
        this.$subscriptions.push(this.headerService.createOPA(this.commonService.getCurrentUserDetail('personID'),
            this.commonService.getCurrentUserDetail('homeUnit'))
            .subscribe((res: any) => {
                if(res) {
                    this.router.navigate([OPA_REDIRECT_URL], {queryParams: {disclosureId: res.disclosureId}});
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, please try again.');
                }
            }, err => this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, please try again.')));
    }

    createConsultingDisclosure() {
        this.$subscriptions.push(this.headerService.createConsultingForm(this.commonService.getCurrentUserDetail('personID'),
            this.commonService.getCurrentUserDetail('homeUnit'))
            .subscribe((res: any) => {
                if(res) {
                    this.router.navigate([CONSULTING_REDIRECT_URL], {queryParams: {disclosureId: res.disclosureId}});
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, please try again.');
                }
            }, err => this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, please try again.')
        ));
    }

    changeTheme(themename: string) {
        document.querySelector("html").className = '';
        document.querySelector("html").classList.add(themename);
        $('#dissmiss-btn').click();
    }

    openPersonDetailsModal(): void {
        this.commonService.openPersonDetailsModal(this.commonService.getCurrentUserDetail('personID'));
    }


    ngAfterViewInit() {
        // //  Get a reference to the body element
        // const BODY_ELEMENT = this.elementRef.nativeElement.ownerDocument.body;
        // //  Use MutationObserver to observe changes in the body element
        // const HEADER = document.getElementById('coi-header-card');
        // const OBSERVER = new MutationObserver((mutations) => {
        //     mutations.forEach((mutation) => {
        //         if(HEADER) {
        //             if (document.body.style.overflowY === 'hidden' ) {
        //                 HEADER.style.marginRight = '12px';
        //                 document.body.style.marginRight = '12px';
        //             } else {
        //                 HEADER.style.marginRight = '0';
        //                 document.body.style.marginRight = '0';
        //             }
        //         }
        //     });
        // });
        // // Configure and start the OBSERVER
        // const CONFIG = { attributes: true, childList: true, subtree: true };
        // OBSERVER.observe(BODY_ELEMENT, CONFIG);
    }

    // for closing notes popup while clicking escape key
    @HostListener('document:keydown.escape', ['$event'])
    handleEscapeEvent(event: any): void {
        if ((event.key === 'Escape' || event.key === 'Esc')) {
            if (this.commonService.isShowCreateNoteModal) {
                this.closeAddNote();
            }
        }
    }

}
