
import { Component, EventEmitter, Input, OnChanges, OnInit, Output, SimpleChanges } from '@angular/core';
import { CustomElementVO, FormBuilder, FormBuilderEvent, FormBuilderSaveRO, QuestionnaireVO,
        FBConfiguration } from './form-builder-interface';
import { Observable, Subject } from 'rxjs';
import { FormBuilderService } from './form-builder.service';

@Component({
    selector: 'app-form-builder-view',
    templateUrl: './form-builder-view.component.html',
    styleUrls: ['./form-builder-view.component.scss'],
    providers: [FormBuilderService]
})
export class FormBuilderViewComponent implements OnInit, OnChanges {

    @Input() externalEvents: Observable<FormBuilderEvent>;
    @Output() builderStatus = new EventEmitter<string>();
    formBuilderData = new FormBuilder();
    isSubscribed = false;
    saveEventForChildComponent = new Subject();
    fbConfiguration = new FBConfiguration();

    constructor( private  _formBuilderService: FormBuilderService) {}

    ngOnChanges(changes: SimpleChanges): void {
        this.subscribeToExternalEvents();
        this.isSubscribed = true;
    }

    ngOnInit(): void {
        this.builderStatus.emit('READY');
    }

    private subscribeToExternalEvents(): void {
        if (!this.isSubscribed) {
            this.externalEvents.subscribe((E: FormBuilderEvent) => {
                if (E.eventType === 'CONFIGURATION') {
                    this.fbConfiguration = E.data;
                    this.getFormBuilderData();
                }
                if (E.eventType === 'SAVE') {
                    this.saveEventForChildComponent.next({eventType: 'EXTERNAL_SAVE'});
                }
                if (E.eventType === 'SAVE_COMPLETED') {
                    this.saveEventForChildComponent.next({eventType: 'CHANGE_FLAG', data: false});
                }
            });
        }
    }

    private getFormBuilderData(): void {
        this._formBuilderService.getFormBuilderData(this.fbConfiguration).subscribe((data: any) => {
            this.formBuilderData = data;
        });
    }


}
