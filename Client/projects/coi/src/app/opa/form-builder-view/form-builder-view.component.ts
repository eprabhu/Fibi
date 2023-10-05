import {Component, Input, OnChanges, OnInit, SimpleChanges} from '@angular/core';
import { FormBuilder } from '../opa-interface';

@Component({
    selector: 'app-form-builder-view',
    templateUrl: './form-builder-view.component.html',
    styleUrls: ['./form-builder-view.component.scss']
})
export class FormBuilderViewComponent implements OnChanges, OnInit {

    @Input()formBuilderData: FormBuilder;

    ngOnInit(): void {
    }

    ngOnChanges(changes: SimpleChanges): void {
        console.log(this.formBuilderData);
    }


}
