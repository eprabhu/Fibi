import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router, RouterStateSnapshot } from '@angular/router';
import { CommonService } from '../common.service';
import { getPathWithoutParams, getSpecificUrlPart } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { ADMIN_DASHBOARD_RIGHTS, COI_CONFIGURATIONS_RIGHTS, COI_DISCLOSURE_SUPER_ADMIN_RIGHTS, ENTITY_RIGHTS, OPA_DISCLOSURE_ADMIN_RIGHTS, OPA_DISCLOSURE_RIGHTS } from '../../../app-constants';

@Injectable()
export class AdminRouteGuardService {

    constructor(private _router: Router, public _commonService: CommonService) { }

    async canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): Promise<boolean> {
        localStorage.setItem('currentUrl', state.url);
        return await this.isPathRightAccessible(state);
    }

    private async isPathRightAccessible(state: RouterStateSnapshot): Promise<boolean> {
        if (await this.isPathAllowed(state)) {
            return true;
        } else {
            this._router.navigate(['/coi/error-handler/403']);
            return false;
        }
    }


    private async isPathAllowed(state: RouterStateSnapshot): Promise<boolean> {
        const path = getPathWithoutParams(getSpecificUrlPart(state.url, 2));
        if (this.isRightCheckingNeeded(path)) { return await this.hasPathRights(path); }
        return true;
    }

    private isRightCheckingNeeded(pathName: string): boolean {
        if (!pathName) { return false; }
        const PATHS = ['admin-dashboard', 'opa-dashboard', 'entity-dashboard', 'configuration'];
        return PATHS.includes(pathName);
    }

    private async hasPathRights(path: string): Promise<boolean> {
        switch (path) {
            case 'entity-dashboard': return this._commonService.getAvailableRight(ENTITY_RIGHTS);
            case 'configuration': return this._commonService.getAvailableRight(COI_CONFIGURATIONS_RIGHTS);
            case 'opa-dashboard': return this._commonService.checkOPARights();
            case 'admin-dashboard': return this._commonService.checkFCOIRights();
            default: return true;
        }
    }

}
