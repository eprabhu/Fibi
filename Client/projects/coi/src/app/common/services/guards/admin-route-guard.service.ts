import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router, RouterStateSnapshot } from '@angular/router';
import { CommonService } from '../common.service';
import { getPathWithoutParams } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { ADMIN_DASHBOARD_RIGHTS, COI_DISCLOSURE_SUPER_ADMIN_RIGHTS } from '../../../app-constants';

@Injectable()
export class AdminRouteGuardService {

    constructor(private _router: Router, public _commonService: CommonService) { }

    async canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): Promise<boolean> {
        localStorage.setItem('currentUrl', state.url);
        return await this.isPathRightAccessible();
    }

    private async isPathRightAccessible(): Promise<boolean> {
        if (await this.isPathAllowed()) {
            return true;
        } else {
            this._router.navigate(['/coi/error-handler/403']);
            return false;
        }
    }

    private async isPathAllowed(): Promise<boolean> {
        return await this.hasPathRights();
    }

    private async hasPathRights(): Promise<boolean> {
        const isAdmin = this._commonService.rightsArray.some((right) => ADMIN_DASHBOARD_RIGHTS.has(right)) || this._commonService.getAvailableRight(COI_DISCLOSURE_SUPER_ADMIN_RIGHTS);
        return isAdmin || this._commonService.isCoiReviewer;
    }

}
