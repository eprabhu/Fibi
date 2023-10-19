import {Component, OnDestroy, OnInit, ViewEncapsulation} from '@angular/core';
import {environment} from '../../../environments/environment';
import {Router} from '@angular/router';
import {CommonService} from '../services/common.service';
import {Subscription} from 'rxjs';
import {subscriptionHandler} from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HeaderService } from './header.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';

class ChangePassword {
    password = '';
    reEnterPassword = '';
}

@Component({
    selector: 'app-header',
    templateUrl: './header.component.html',
    styleUrls: ['./header.component.scss'],
    providers: [HeaderService],
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
    isShowCreateNoteModal = false;
    noteComment: any;
    isOpenAttachmentModal = false;

    constructor(public _router: Router, public commonService: CommonService, private _headerService: HeaderService) {
        this.logo = environment.deployUrl + './assets/images/logo.png';
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
    }

    redirectToOpa() {
        this._router.navigate(['/coi/opa/form'],
            {queryParams: {disclosureId: 2}});
    }

    navigateForHomeIcon(): void {
        this.homeNavigation = this.isAdministrator ? '#/coi/admin-dashboard' : '#/coi/user-dashboard/disclosures';
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    logout() {
        this._router.navigate(['/logout']);
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

    createOPA() {
        this.$subscriptions.push(this._headerService.createOPA(this.commonService.getCurrentUserDetail('personId'),
            this.commonService.getCurrentUserDetail('homeUnit'))
            .subscribe((res: any) => {
                this._router.navigate(['/coi/opa/form'], {queryParams: {disclosureId: res.opaDisclosureId}});
            }));
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
            this.$subscriptions.push(this._headerService.saveOrUpdatePersonNote({
                'personId': this.commonService.getCurrentUserDetail('personId'),
                'content': this.noteComment.trim()
            }).subscribe((ele: any) => {
                this.isShowCreateNoteModal = false;
                this.noteComment = '';
                if(this._router.url.includes('/coi/user-dashboard/notes')) {
                    this.commonService.$updateLatestNote.next(ele);
                }
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Note added successfully.');
            } , error => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in adding note, please try again.');
            }));
        }
    }

    showNotes() {
        this.isShowCreateNoteModal = true;
        setTimeout(() => {
            document.getElementById("textArea").focus();
        });
    }

    closeAddNote() {
        this.isShowCreateNoteModal = false;
        this.noteComment = '';
    }

    closeModal() {
        this.isOpenAttachmentModal = false;
    }
}
