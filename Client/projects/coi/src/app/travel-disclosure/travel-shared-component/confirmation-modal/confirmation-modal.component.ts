import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { TravelDisclosureService } from '../../services/travel-disclosure.service';

@Component({
    selector: 'app-confirmation-modal',
    templateUrl: './confirmation-modal.component.html',
    styleUrls: ['./confirmation-modal.component.scss']
})
export class ConfirmationModalComponent implements OnInit {

    @Input() modalHeaderInfo: string;
    @Input() isExitsDescription = false;
    @Output() btnAction: EventEmitter<any> = new EventEmitter<any>();
    description = '';

    constructor(public service: TravelDisclosureService) { }

    ngOnInit() {
    }

    performAction() {
        this.btnAction.emit(this.isExitsDescription ? this.description : null);
    }

}
