open Jest;

open Expect;

describe("Demo", () =>
  test("#hello", () =>
    expect(Demo.hello()) |> toEqual("hello")
  )
);
