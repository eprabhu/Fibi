import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { CommonService } from '../common.service';

@Injectable()
export class LoginGuard {
    constructor(private _router: Router, public _commonService: CommonService) { }
    canActivate(): boolean {
        if (!this._commonService.enableSSO) {
            return true;
        } else {
            this._router.navigate(['error/401']);
            return false;
        }
}

}
