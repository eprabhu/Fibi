import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router, RouterStateSnapshot } from '@angular/router';
import { CommonService } from '../common.service';
import { getPathWithoutParams } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { ADMIN_DASHBOARD_RIGHTS } from '../../../app-constants';

@Injectable()
export class AdminRouteGuardService {

    constructor(private _router: Router, public _commonService: CommonService) { }

    async canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): Promise<boolean> {
        localStorage.setItem('currentUrl', state.url);
        const authToken = this._commonService.getCurrentUserDetail('Authorization');
        if (authToken) {
            return await this.isPathRightAccessible();
        } else {
            this._router.navigate(['/login']);
            return false;
        }
    }

    private async isPathRightAccessible(): Promise<boolean> {
        if (await this.isPathAllowed()) {
            return true;
        } else {
            this._router.navigate(['/fibi/error/403']);
            return false;
        }
    }

    private async isPathAllowed(): Promise<boolean> {
        return await this.hasPathRights();
    }

    private async hasPathRights(): Promise<boolean> {
        return await this.checkIfRightsPresent(ADMIN_DASHBOARD_RIGHTS);
    }

    async checkIfRightsPresent(adminDashboardRights: Set<string>): Promise<boolean> {
        const rightsArray = await this._commonService.fetchPermissions();
        return rightsArray.some((right) => adminDashboardRights.has(right));
    }

}
