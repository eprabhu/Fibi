import { Injectable } from '@angular/core';
import { DataStoreService } from './data-store.service';

@Injectable()
export class RouterGuardService  {

    constructor(private _dataStore: DataStoreService) { }

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
