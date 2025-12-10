namespace Domain

open System
open System.Text.RegularExpressions


type EmailError = 
    | InvalidFormat of string
    | Empty of string

type PhoneError = 
    | InvalidFormat of string
    | Empty of string

type BirthYearError =
    | ToOld of int
    | InTheFuture of int

type ApplicationError =
    | EmailValidationError of EmailError
    | PhoneValidationError of PhoneError
    | BirthYearValidationError of BirthYearError


module RegexMatching =
    type Pattern =
        | Email
        | PhoneNumber

    let validate (pattern: Pattern) (value: string) =
        let rx =
            match pattern with
            | Email -> Regex(@"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$")
            | PhoneNumber -> Regex(@"^(?:\+46\s?|0)\d(?:[\s]?\d){8,11}$")

        rx.IsMatch(value)



module Email =
    type Email = private Email of string

    let create value: Result<Email, ApplicationError> =
        match value with
        | null
        | "" -> Error (ApplicationError.EmailValidationError (EmailError.Empty value))
        | s when String.IsNullOrWhiteSpace s -> Error (ApplicationError.EmailValidationError (EmailError.Empty value))
        | s when not (RegexMatching.validate RegexMatching.Pattern.Email s) -> Error (ApplicationError.EmailValidationError (EmailError.InvalidFormat value))
        | s -> Ok(Email s)

    let value (Email string) = string


module Phone =
    type Phone = private Phone of string

    let create value: Result<Phone, ApplicationError> =
        match value with
        | null
        | "" -> Error (ApplicationError.PhoneValidationError (PhoneError.Empty value))
        | s when String.IsNullOrWhiteSpace s -> Error (ApplicationError.PhoneValidationError (PhoneError.Empty value))
        | s when not (RegexMatching.validate RegexMatching.Pattern.PhoneNumber s) -> Error (ApplicationError.PhoneValidationError (PhoneError.InvalidFormat value))
        | s -> Ok(Phone s)

    let value (Phone string) = string


module BirthYear =
    type BirthYear = private BirthYear of int

    let create value: Result<BirthYear, ApplicationError> =
        let currentYear = System.DateTime.Now.Year

        match value with
        | y when y < 1900 -> Error (ApplicationError.BirthYearValidationError (BirthYearError.ToOld value))
        | y when y > currentYear -> Error (ApplicationError.BirthYearValidationError (BirthYearError.InTheFuture value))
        | y -> Ok(BirthYear y)

    let value (BirthYear year) = year


type Address = {
    Street: string
    City: string
}

type MemberApplication = {
    Id: System.Guid
    FirstName: string
    LastName: string
    Email: Email.Email
    Phone: Phone.Phone
    Address: Address
    BirthYear: BirthYear.BirthYear
}

type CreateMemberRequest = {
    FirstName: string
    LastName: string
    Email: string
    Phone: string
    Street: string
    City: string
    BirthYear: int
}

module MemberApplication =
    let create (req: CreateMemberRequest): Result<MemberApplication, ApplicationError> =
        // Validate all fields
        match Email.create req.Email, Phone.create req.Phone, BirthYear.create req.BirthYear with
        | Ok email, Ok phone, Ok birthYear ->
            Ok {
                Id = System.Guid.NewGuid()
                FirstName = req.FirstName
                LastName = req.LastName
                Email = email
                Phone = phone
                Address = { Street = req.Street; City = req.City }
                BirthYear = birthYear
            }
        | Error e, _, _ -> Error e
        | _, Error e, _ -> Error e
        | _, _, Error e -> Error e