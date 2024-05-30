import { Component, Input, OnInit } from '@angular/core';
import { InformationAndHelpTextService } from '../../common/services/informationAndHelpText.service';
import * as bootstrap from 'bootstrap';

@Component({
    selector: 'app-common-help-text',
    templateUrl: './common-help-text.component.html',
    styleUrls: ['./common-help-text.component.scss'],
})
export class CommonHelpTextComponent implements OnInit {

    @Input() elementId: any = '';
    @Input() subSectionId: any = '';
    @Input() placement: 'left' | 'right' | 'top' | 'bottom' = 'right';

    helpText: string = '';

    constructor(private _informationAndHelpText: InformationAndHelpTextService) { }

    ngOnInit() {
        this.getInformation();
    }

    ngAfterViewInit() {
        const tooltipTriggerList: any = document.querySelectorAll('[data-bs-toggle="tooltip"]');
        const tooltipList = [...tooltipTriggerList].map(tooltipTriggerEl => new bootstrap.Tooltip(tooltipTriggerEl))
    }

    getInformation() {
        this.helpText = this._informationAndHelpText.getHelpText(this.subSectionId, this.elementId);
    }

}

