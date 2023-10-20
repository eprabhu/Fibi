import { subscriptionHandler } from './../../../../../fibi/src/app/common/utilities/subscription-handler';

import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output, SimpleChanges } from '@angular/core';
import { FormBuilder, FormBuilderEvent, FBConfiguration } from './form-builder-interface';
import { Observable, Subject } from 'rxjs';
import { FormBuilderService } from './form-builder.service';

@Component({
    selector: 'app-form-builder-view',
    templateUrl: './form-builder-view.component.html',
    styleUrls: ['./form-builder-view.component.scss'],
    providers: [FormBuilderService]
})
export class FormBuilderViewComponent implements OnInit, OnChanges, OnDestroy {

    @Input() externalEvents: Observable<FormBuilderEvent>;
    @Output() builderStatus = new EventEmitter<string>();
    formBuilderData = new FormBuilder();
    isSubscribed = false;
    saveEventForChildComponent = new Subject();
    fbConfiguration = new FBConfiguration();
    subscription$ = [];
    isFormEditable = true;

    constructor( private  _formBuilderService: FormBuilderService) {}

    ngOnChanges(changes: SimpleChanges): void {
        this.subscribeToExternalEvents();
        this.isSubscribed = true;
    }

    ngOnInit(): void {
        this.builderStatus.emit('READY');
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.subscription$);
    }

    private subscribeToExternalEvents(): void {
        if (!this.isSubscribed) {
            this.subscription$.push(this.externalEvents.subscribe((E: FormBuilderEvent) => {
                if (E.eventType === 'CONFIGURATION') {
                    this.fbConfiguration = E.data;
                    this.getFormBuilderData();
                } else if (E.eventType === 'SAVE') {
                    this.saveEventForChildComponent.next({eventType: 'EXTERNAL_SAVE'});
                } else if (E.eventType === 'SAVE_COMPLETED') {
                    this.saveEventForChildComponent.next({eventType: 'CHANGE_FLAG', data: false});
                } else if (E.eventType === 'IS_EDIT_MODE') {
                    this.isFormEditable = E.data;
                }
            }));
        }
    }

    private getFormBuilderData(): void {
        this._formBuilderService.getFormBuilderData(this.fbConfiguration).subscribe((data: any) => {
            this.formBuilderData = data;
        });
    }


}
