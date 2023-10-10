import {AfterViewInit, Component} from '@angular/core';
import {Subject} from 'rxjs';
import {FBConfiguration, FormBuilderEvent} from '../../shared/form-builder-view/form-builder-interface';

@Component({
    selector: 'app-form',
    templateUrl: './form.component.html',
    styleUrls: ['./form.component.scss']
})
export class FormComponent implements AfterViewInit {

    formBuilderEvents = new Subject<FormBuilderEvent>();
    fbConfiguration = new FBConfiguration();

    constructor() {
    }

    ngAfterViewInit(): void {
        // NEEDS TO SETUP FORM BUILDER DATA HERE, currently adding dummy data fro save testing
        this.fbConfiguration.moduleItemCode = '23';
        this.fbConfiguration.moduleSubItemCode = '0';
        this.fbConfiguration.moduleItemKey = '11000110';
        this.fbConfiguration.moduleSubItemKey = '0';
        this.fbConfiguration.documentOwnerPersonId = '110000001';
        this.formBuilderEvents.next({eventType: 'CONFIGURATION', data: this.fbConfiguration});
    }

    triggerSave() {
        this.formBuilderEvents.next({eventType: 'SAVE'});
    }


}
