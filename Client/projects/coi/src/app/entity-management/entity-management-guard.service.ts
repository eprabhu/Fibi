import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';
import { CommonService } from '../common/services/common.service';

@Injectable()

export class EntityManagementGuardService implements CanActivate {

    constructor(private router: Router, private _commonService: CommonService) { }

    canActivate(): any {
        if (this._commonService.hasRight('MANAGE_ENTITY') || this._commonService.hasRight('MANAGE_ENTITY')) {
            return true;
        } else {
            this.router.navigate(['/coi/error-handler/401']);
            return false;
        }
    }
}

