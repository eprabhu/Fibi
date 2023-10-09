import {  } from './../shared/questionnaire-list-compare/interface';
import {AfterViewInit, Component, OnInit} from '@angular/core';
import { FBConfiguration, FormBuilder, FormBuilderEvent } from '../shared/form-builder-view/form-builder-interface';
import { OpaService } from './opa-service.service';
import { Subject } from 'rxjs';

@Component({
    selector: 'app-opa',
    templateUrl: './opa.component.html',
    styleUrls: ['./opa.component.scss'],
    providers: [OpaService]
})
export class OpaComponent implements OnInit, AfterViewInit {
    isCardExpanded = true;
    formBuilderData: FormBuilder = new FormBuilder();
    formBuilderEvents = new Subject<FormBuilderEvent>();
    fbConfiguration = new FBConfiguration();
    constructor(private _opa: OpaService ) {}

    ngOnInit(): void {
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
