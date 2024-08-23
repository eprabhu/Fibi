import { Component } from '@angular/core';
import { AutoSaveService } from '../common/services/auto-save.service';

@Component({
  selector: 'app-entity-management-module',
  templateUrl: './entity-management-module.component.html',
  styleUrls: ['./entity-management-module.component.scss']
})
export class EntityManagementModuleComponent {

    result: any;

    constructor(public _autoSaveService: AutoSaveService) {}

    ngOnInit() {
        this._autoSaveService.initiateAutoSave();
        // this.result = this._route.snapshot.data.agreementDetails;
    }

    triggerAutoSave() {
        this._autoSaveService.commonSaveTrigger$.next(true);
    }

}
