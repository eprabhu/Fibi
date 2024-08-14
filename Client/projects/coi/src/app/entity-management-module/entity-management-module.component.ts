import { Component } from '@angular/core';

@Component({
  selector: 'app-entity-management-module',
  templateUrl: './entity-management-module.component.html',
  styleUrls: ['./entity-management-module.component.scss']
})
export class EntityManagementModuleComponent {

    result: any;

    constructor() {}

    ngOnInit() {
        // this.result = this._route.snapshot.data.agreementDetails;
    }

}
