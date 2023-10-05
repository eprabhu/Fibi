import {Component, OnInit} from '@angular/core';
import { FormBuilder } from './opa-interface';
import { OpaService } from './opa-service.service';

@Component({
    selector: 'app-opa',
    templateUrl: './opa.component.html',
    styleUrls: ['./opa.component.scss'],
    providers: [OpaService]
})
export class OpaComponent implements OnInit {
    isCardExpanded = true;
    formBuilderData: FormBuilder = new FormBuilder();

    constructor(private _opa: OpaService ) {}

    ngOnInit(): void {
        this.getFormBuilderData();
    }

    getFormBuilderData(): void {
        this._opa.getFormBuilderData().subscribe((data: FormBuilder) => {
            this.formBuilderData = data;
        });
    }


}
