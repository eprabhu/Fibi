import { Component } from '@angular/core';
import { CommonService } from '../common/services/common.service';
import { Router } from '@angular/router';

@Component({
    selector: 'app-logout',
    templateUrl: './logout.component.html',
    styleUrls: ['./logout.component.scss']
})

export class LogoutComponent {
    constructor(private _commonService: CommonService, private _router: Router) { }

    async login() {
        if (this._commonService.enableSSO) {
            try {
                const loginDetails: any = await this._commonService.authLogin();
                this._commonService.onSuccessFullLogin(loginDetails);
            } catch (e) {
                this._commonService.onFailedLogin(e);
            }
        } else {
                this._router.navigate(['/login']);
        }
    }

}
