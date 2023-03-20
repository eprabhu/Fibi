import { NgModule } from '@angular/core';
import {AppAutocompleterComponent} from "./app-autocompleter/app-autocompleter.component";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";

@NgModule({
  declarations: [
    AppAutocompleterComponent,
  ],
  imports: [
      FormsModule,
      CommonModule
  ],
  exports: [
    AppAutocompleterComponent
  ]
})
export class SharedLibraryModule { }
