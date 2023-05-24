import { ActivatedRouteSnapshot, CanActivate, CanDeactivate, Router, RouterStateSnapshot } from "@angular/router";
import { Injectable } from "@angular/core";
import { DataStoreService } from "../disclosure/services/data-store.service";


@Injectable()
export class travelRouteGuardService implements CanActivate, CanDeactivate<boolean> {
    grantId = null;
    constructor(private _dataStore: DataStoreService, private _router: Router) { }

    canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
        return true;
    }

    canDeactivate(): boolean {
        if (this._dataStore.dataChanged) {
            document.getElementById('hidden-validate-button').click();
            return false;
        }
        else {
            return true;
        }
    }
}