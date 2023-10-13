import {AfterViewInit, Component} from '@angular/core';
import {Subject} from 'rxjs';
import {FBConfiguration, FormBuilderEvent} from '../../shared/form-builder-view/form-builder-interface';
import { OpaService } from '../services/opa.service';

@Component({
    selector: 'app-form',
    templateUrl: './form.component.html',
    styleUrls: ['./form.component.scss']
})
export class FormComponent implements AfterViewInit {

    formBuilderEvents = new Subject<FormBuilderEvent>();
    fbConfiguration = new FBConfiguration();

    constructor(public _opa: OpaService) {
    }

    ngAfterViewInit(): void {
        // NEEDS TO SETUP FORM BUILDER DATA HERE, currently adding dummy data fro save testing
        this.fbConfiguration.moduleItemCode = '23';
        this.fbConfiguration.moduleSubItemCode = '0';
        this.fbConfiguration.moduleItemKey = '11000111';
        this.fbConfiguration.moduleSubItemKey = '0';
        this.fbConfiguration.documentOwnerPersonId = '110000001';
        this.fbConfiguration.formBuilderId = 2;
        this._opa.formBuilderEvents.next({eventType: 'CONFIGURATION', data: this.fbConfiguration});
    }

    triggerSave() {
        this._opa.formBuilderEvents.next({eventType: 'SAVE'});
    }


}
