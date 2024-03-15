import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';
import { CommonService } from '../common/services/common.service';

@Injectable()

export class EntityManagementGuardService implements CanActivate {

    constructor(private router: Router, private _commonService: CommonService) { }

    canActivate(): any {
        if (this._commonService.getAvailableRight(['MANAGE_ENTITY', 'VIEW_ENTITY'], 'SOME')) {
            return true;
        } else {
            this.router.navigate(['/coi/error-handler/403']);
            return false;
        }
    }
}

