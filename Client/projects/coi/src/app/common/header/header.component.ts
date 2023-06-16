import {Component, OnDestroy, OnInit, ViewEncapsulation} from '@angular/core';
import {environment} from '../../../environments/environment';
import {Router} from '@angular/router';
import {CommonService} from '../services/common.service';
import {Subscription} from 'rxjs';
import {subscriptionHandler} from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import {ADMIN_DASHBOARD_RIGHTS} from '../../app-constants';

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
    isManageEntity = false;
    isShowAdminDashboard = false;
    canViewAdminDashboard = false;

    constructor(public _router: Router, public commonService: CommonService) {
        this.logo = environment.deployUrl + './assets/images/logo.png';
    }

    ngOnInit() {
        this.fullName = this.commonService.getCurrentUserDetail('fullName');
        this.checkUserHasRight();
        this.getPermissions();
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

    private passwordAtleast7Characters() {
        if (this.resetPassword.password.length < 7) {
            this.passwordValidation.set('password-length', true);
        }
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

    private confirmPasswordSame() {
        if (this.resetPassword.password !== this.resetPassword.reEnterPassword) {
            this.passwordValidation.set('same-password', true);
        }
    }

    checkUserHasRight(): void {
        this.isManageEntity = this.commonService.getAvailableRight(['MANAGE_ENTITY', 'VIEW_ENTITY'], 'SOME');
        this.canViewAdminDashboard = this.commonService.getAvailableRight(['APPLICATION_ADMINISTRATOR',
                'MAINTAIN_QUESTIONNAIRE', 'MAINTAIN_USER_ROLES', 'MAINTAIN_ROLE', 'MAINTAIN_PERSON', 'MAINTAIN_TRAINING',
                'VIEW_KEY_PERSON_TIMESHEET', 'MAINTAIN_KEY_PERSON_TIMESHEET', 'MAINTAIN_DELEGATION', 'MAINTAIN_ORCID_WORKS'],
            'SOME');
    }

    async getPermissions() {
        const rightsArray = await this.commonService.fetchPermissions();
        this.isShowAdminDashboard = rightsArray.some((right) => ADMIN_DASHBOARD_RIGHTS.has(right));
    }
}
