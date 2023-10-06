
import { Component, Input, OnChanges, OnInit, SimpleChanges } from '@angular/core';
import { FormSection } from '../../opa-interface';

@Component({
  selector: 'app-form-sections',
  templateUrl: './form-sections.component.html',
  styleUrls: ['./form-sections.component.scss']
})
export class FormSectionsComponent implements OnInit, OnChanges {


  @Input() sectionDetails  = new FormSection();
  constructor() { }


  ngOnInit() {
  }

  ngOnChanges(changes: SimpleChanges): void {
    console.log(this.sectionDetails);
  }

}
