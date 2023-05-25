import { CanActivate, CanDeactivate } from "@angular/router";
import { Injectable } from "@angular/core";
import { DataStoreService } from "../disclosure/services/data-store.service";

@Injectable()
export class travelRouteGuardService implements CanActivate, CanDeactivate<boolean> {

    constructor(private _dataStore: DataStoreService) { }

    canActivate(): boolean {
        return true;
    }

    canDeactivate(): boolean {
        if (this._dataStore.dataChanged) {
            document.getElementById('hidden-validate-button').click();
            return false;
        } else {
            return true;
        }
    }
}