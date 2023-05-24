import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { DataStoreService } from './data-store.service';

@Injectable()
export class RouterGuardService  {

    constructor(private _dataStore: DataStoreService, private _router: Router) { }

    canActivate(): boolean {
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
