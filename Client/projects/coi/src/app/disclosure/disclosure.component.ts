import { Component } from '@angular/core';
import { SfiService } from './sfi/sfi.service';

@Component({
    selector: 'app-disclosure',
    templateUrl: './disclosure.component.html',
    styleUrls: ['./disclosure.component.scss']
})
export class DisclosureComponent {

    constructor(public sfiService: SfiService) { }
}
